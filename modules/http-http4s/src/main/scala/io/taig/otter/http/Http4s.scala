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
    */
  def routes[F[_]: Concurrent](payload: Http4sPayload)(routes: Route[F, ?, ?]*): HttpRoutes[F] =
    val decoder = Http4sRequestDecoder(payload)
    val encoder = Http4sResultEncoder(payload)
    val chain = Chain.fromSeq(routes)

    HttpRoutes[F]: request =>
      val method = Http4sEnvelope.toMethod(request.method)
      val segments = Http4sEnvelope.toPath(request.uri.path)

      OptionT
        .fromOption[F](chain.find(_.matches(method, segments)))
        .semiflatMap(_.run(decoder, encoder, request, segments))

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

  /** What a request that this endpoint described, but that did not hold what it described, is answered with.
    *
    * `400` and a plain text report. Plain text because a violation report is not a payload the endpoint declared, so
    * answering in the endpoint's own alphabet would be describing something the document does not mention; and because
    * a module that renders JSON would need a JSON interpreter, which is exactly the dependency the payload trait was
    * made open to avoid.
    */
  private[http] def malformed(violations: Violations): Either[Http4sIssue, Http4sWire.Response] =
    val bytes = ByteVector.encodeUtf8(Http4s.report(violations)).getOrElse(ByteVector.empty)

    Right(Http4sWire.Response(Code.BadRequest, Chain.empty, Some((MediaType.Text, bytes))))

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
        val body = Option.when(bytes.nonEmpty)(
          (Http4sEnvelope.toMediaType(response.headers).getOrElse(MediaType.OctetStream), bytes)
        )

        (Http4sEnvelope.toHeaders(response.headers), body)
