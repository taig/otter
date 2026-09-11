package io.taig.otter.http

import cats.data.Chain
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.all.*
import io.taig.otter.Step
import io.taig.otter.Violations
import io.taig.otter.http.codec.Http4sPayload
import io.taig.otter.http.codec.Http4sRequestDecoder
import io.taig.otter.http.codec.Http4sRequestEncoder
import io.taig.otter.http.codec.Http4sResultDecoder
import io.taig.otter.http.codec.Http4sResultEncoder
import io.taig.otter.http.component.HttpComponent
import io.taig.otter.http.component.MediaTypeComponent
import org.http4s.Entity
import org.http4s.HttpRoutes
import org.http4s.Request as Http4sRequest
import org.http4s.Response as Http4sResponse
import org.http4s.Uri
import org.http4s.client.Client as Http4sClient
import scodec.bits.ByteVector

/** Endpoints, served and called.
  *
  * The bridge object, on the pattern [[io.taig.otter.JsonCirce]] and [[io.taig.otter.JsonBorer]] set: the codecs are
  * the interpreter, and this is where they become the thing a caller actually wanted -- an `HttpRoutes`, a function --
  * and where `Violations` stops being a value and becomes an HTTP fact. Both translations live here and nowhere else,
  * so what a malformed request looks like is one decision recorded in one place.
  */
object Http4s:
  /** A set of routes, tried in the order they are given.
    *
    * `HttpRoutes` and not `HttpApp`, because falling through is the honest answer to a path none of these describe:
    * composing with `<+>` is then somebody else's decision, and so is what a `404` looks like.
    *
    * `malformed` is what a request that this set described but did not hold is answered with, and it is a parameter
    * because that answer is an API's own vocabulary rather than this module's. [[Http4s.malformed]] is the default and
    * says the true thing in plain text; an API whose errors have a schema passes a function that renders one, and keeps
    * its callers reading a single error shape.
    */
  def routes[F[_]: Concurrent](
      payload: Http4sPayload,
      malformed: Violations => Http4sWire.Response = Http4s.malformed
  )(routes: Route[F, ?, ?]*): HttpRoutes[F] =
    val decoder = Http4sRequestDecoder(payload)
    val encoder = Http4sResultEncoder(payload)
    val chain = Chain.fromSeq(routes)

    HttpRoutes[F]: request =>
      val method = Http4sEnvelope.toMethod(request.method)
      val segments = Http4sEnvelope.toPath(request.uri.path)

      OptionT
        .fromOption[F](chain.find(_.matches(method, segments)))
        .semiflatMap(_.run(decoder, encoder, malformed, request, segments))

  /** An endpoint, as a function that calls it.
    *
    * The endpoint is read as a caller sees it -- it writes the request and reads the response -- which is why the same
    * value cannot be handed to [[Http4s.routes]] and this without saying which side it is. That is [[Endpoint.Server]]
    * and [[Endpoint.Client]], and it is checked by the compiler rather than remembered.
    */
  def client[F[_]: Concurrent, A, B](payload: Http4sPayload, base: Uri, client: Http4sClient[F])(
      endpoint: Endpoint.Client[Body.Payload, A, B]
  ): A => F[B] =
    val encoder = Http4sRequestEncoder(payload)
    val decoder = Http4sResultDecoder(payload)

    value =>
      for
        wire <- Http4s.raise[F, Http4sWire.Request](encoder.encode(endpoint.request, value))
        method <- Http4sEnvelope
          .toHttp4sMethod(endpoint.request.method)
          .leftMap(failure => Http4sFailure.Method(endpoint.request.method, failure.message))
          .liftTo[F]
        response <- client
          .run(Http4s.toHttp4sRequest[F](method, base, wire))
          .use(response => Http4s.toWire(response).map((response.status.code, _)))
        decoded <- decoder
          .decode(endpoint.responses, Http4sWire.Response(Code(response._1), response._2._1, response._2._2))
          .leftMap(Http4sFailure.Response.apply)
          .liftTo[F]
      yield decoded

  /** The status a violation report goes out under.
    *
    * RFC 9110 draws the line by what failed rather than by how badly. `400` is a request whose *syntax* the server will
    * not process; `422` is one whose content type is understood and whose content parses, but whose instructions cannot
    * be carried out. A body that is JSON and breaks the schema is squarely the second, and a path segment or a query
    * parameter that will not parse is squarely the first -- there is no content there to be unprocessable, the request
    * line itself is wrong.
    *
    * So the position decides, and the position is already in the tree:
    * [[io.taig.otter.http.codec.Http4sRequestDecoder]] labels each half it reads, and accumulates them, so a request
    * may hold violations under several at once. `422` only when every one of them is under `body`, because a report
    * that also names a query parameter is not describing a request whose syntax was correct.
    */
  def code(violations: Violations): Code = violations match
    case Violations.Namespace(values) if values.keys.forall(_ == Step.Field("body")) =>
      HttpComponent.code.unprocessableEntity
    case _ => HttpComponent.code.badRequest

  /** What a request that this endpoint described, but that did not hold what it described, is answered with.
    *
    * [[Http4s.code]] and a plain text report. Plain text because a violation report is not a payload the endpoint
    * declared, so answering in the endpoint's own alphabet would be describing something the document does not mention;
    * and because a module that renders JSON would need a JSON interpreter, which is exactly the dependency the payload
    * trait was made open to avoid. A caller that wants its own vocabulary passes one to [[Http4s.routes]] rather than
    * being given a second interpreter here.
    */
  def malformed(violations: Violations): Http4sWire.Response =
    val bytes = ByteVector.encodeUtf8(Http4s.report(violations)).getOrElse(ByteVector.empty)

    Http4sWire.Response(Http4s.code(violations), Chain.empty, Some((MediaTypeComponent.text, bytes)))

  /** A violation tree, one line per violation, each named by where it was found and by what was found there.
    *
    * The actual value belongs on the line as much as the constraint does. A report saying only what was expected leaves
    * a caller comparing it against a request it has to reconstruct; saying what arrived as well is what makes the
    * difference readable without one.
    */
  def report(violations: Violations): String =
    def go(prefix: Chain[Step], violations: Violations): Chain[String] = violations match
      case Violations.Root(values, found) =>
        val here = Chain
          .fromSeq(found.toList)
          .map(violation => s"$$${prefix.toList.mkString}: ${violation.constraint.show} (was ${violation.actual.show})")

        here ++ Chain.fromSeq(values.toList).flatMap((step, nested) => go(prefix :+ step, nested))
      case Violations.Namespace(values) =>
        Chain.fromSeq(values.toSortedMap.toList).flatMap((step, nested) => go(prefix :+ step, nested))

    go(Chain.empty, violations).toList.mkString("\n")

  private def respondable[F[_]](response: Http4sWire.Response): Either[Http4sFailure, Http4sResponse[F]] =
    Http4sEnvelope.toHttp4sResponse[F](response).leftMap(failure => Http4sFailure.Code(response.code, failure.message))

  private[http] def respond[F[_]](
      response: Either[Http4sIssue, Http4sWire.Response]
  )(using F: Concurrent[F]): F[Http4sResponse[F]] =
    response.leftMap(Http4sFailure.Interpreter.apply).flatMap(Http4s.respondable[F]).liftTo[F]

  private def raise[F[_], A](value: Either[Http4sIssue, A])(using F: Concurrent[F]): F[A] =
    value.leftMap(Http4sFailure.Interpreter.apply).liftTo[F]

  private def toHttp4sRequest[F[_]](
      method: org.http4s.Method,
      base: Uri,
      wire: Http4sWire.Request
  ): Http4sRequest[F] =
    val path = wire.path.foldLeft(base.path.toAbsolute)(_.addSegment(_))
    val headers = wire.headers ++ Chain.fromOption(wire.body._1.map(mediaType => ("Content-Type", mediaType.render)))

    Http4sRequest[F](
      method = method,
      uri = base.withPath(path).copy(query = Http4sEnvelope.toHttp4sQuery(wire.queries)),
      headers = Http4sEnvelope.toHttp4sHeaders(headers),
      entity = if wire.body._2.isEmpty then Entity.empty else Entity.strict(wire.body._2)
    )

  private def toWire[F[_]: Concurrent](
      response: Http4sResponse[F]
  ): F[(Chain[(String, String)], Option[(MediaType, ByteVector)])] =
    Http4sEnvelope
      .toBytes(response.entity)
      .map: bytes =>
        val mediaType = Http4sEnvelope.toMediaType(response.headers)
        val body =
          Option.when(mediaType.nonEmpty || bytes.nonEmpty)(
            (mediaType.getOrElse(MediaTypeComponent.octetStream), bytes)
          )

        (Http4sEnvelope.toHeaders(response.headers), body)
