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
import io.taig.otter.http.codec.Http4sResponseDecoder
import io.taig.otter.http.codec.Http4sResponseEncoder
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
  * and where failures become the responses declared by each route's error policy.
  */
object Http4s:
  /** A set of routes, tried in the order they are given.
    *
    * `HttpRoutes` and not `HttpApp`, because falling through is the honest answer to a path none of these describe:
    * composing with `<+>` is then somebody else's decision, and so is what a `404` looks like.
    *
    * The routes are named first and the interpreter after them, which is the direction the requirement actually flows:
    * each route says which payload alphabets answering it needs, [[Body.Or]] accumulates them across the whole set, and
    * the interpreter is what has to cover the total. Naming the interpreter first settled what could be served before a
    * single route had been read, and reported a registry that falls short as a complaint about whichever route first
    * noticed rather than about the interpreter that is missing.
    *
    * Each route carries its declared error policy. `observe` receives diagnostics without choosing responses. It is a
    * second overload of each shape rather than a default argument, because Scala permits default arguments on only one
    * variant of an overloaded method.
    */
  def routes[F[_]: Concurrent]: Http4s.RoutesBuilder[F] = new Http4s.RoutesBuilder[F]

  final class RoutesBuilder[F[_]: Concurrent]:
    def apply[P[-_, +_]](routes: Route[F, Http4sPayload.Supported[P], ?, ?]*)(
        payload: Http4sPayload[P]
    ): HttpRoutes[F] = apply(Routes(routes*))(payload)

    def apply[P[-_, +_]](routes: Route[F, Http4sPayload.Supported[P], ?, ?]*)(
        payload: Http4sPayload[P],
        observe: Http4sObservation[F] => F[Unit]
    ): HttpRoutes[F] = apply(Routes(routes*))(payload, observe)

    /** Apply the API defaults and each route's endpoint-local overrides. */
    def apply[P[-_, +_], E](
        api: Api[Http4sPayload.Supported[P], E],
        routes: Route[F, Http4sPayload.Supported[P], ?, ?]*
    )(payload: Http4sPayload[P]): HttpRoutes[F] = apply(Http4s.composed(api, routes))(payload)

    def apply[P[-_, +_], E](
        api: Api[Http4sPayload.Supported[P], E],
        routes: Route[F, Http4sPayload.Supported[P], ?, ?]*
    )(payload: Http4sPayload[P], observe: Http4sObservation[F] => F[Unit]): HttpRoutes[F] =
      apply(Http4s.composed(api, routes))(payload, observe)

    def apply[P[-_, +_]](routes: Routes[F, Http4sPayload.Supported[P]])(
        payload: Http4sPayload[P]
    ): HttpRoutes[F] = apply(routes)(payload, (_: Http4sObservation[F]) => Concurrent[F].unit)

    def apply[P[-_, +_]](routes: Routes[F, Http4sPayload.Supported[P]])(
        payload: Http4sPayload[P],
        observe: Http4sObservation[F] => F[Unit]
    ): HttpRoutes[F] =
      val decoder = Http4sRequestDecoder(payload)
      val encoder = Http4sResponseEncoder(payload)

      HttpRoutes[F]: request =>
        val method = Http4sEnvelope.toMethod(request.method)
        val segments = Http4sEnvelope.toPath(request.uri.path)

        OptionT
          .fromOption[F](routes.values.find(_.matches(method, segments)))
          .semiflatMap(Route.run(_, decoder, encoder, observe, request, segments))

  /** Each route under the API's global policy, which its own overrides are applied on top of. */
  private def composed[F[_], P[-_, +_], E](
      api: Api[Http4sPayload.Supported[P], E],
      routes: Seq[Route[F, Http4sPayload.Supported[P], ?, ?]]
  ): Routes[F, Http4sPayload.Supported[P]] =
    Routes(routes.map(route => route.copy(errors = route.overrides(api.errors)))*)

  /** An endpoint, as a function that calls it.
    *
    * The endpoint is read as a caller sees it -- it writes the request and reads the response -- which is why the same
    * value cannot be handed to [[Http4s.routes]] and this without saying which side it is. That is [[Endpoint.Server]]
    * and [[Endpoint.Client]], and it is checked by the compiler rather than remembered.
    *
    * The endpoint comes first and the interpreter after it, for the reason [[Http4s.routes]] takes its routes first.
    */
  def client[F[_]: Concurrent, A, B]: Http4s.ClientBuilder[F, A, B] = new Http4s.ClientBuilder[F, A, B]

  final class ClientBuilder[F[_]: Concurrent, A, B]:
    def apply[P[-_, +_], E, D](
        api: Api[Http4sPayload.Supported[P], E],
        endpoint: Endpoint.Declaration[Http4sPayload.Supported[P], A, Any, Nothing, B, D]
    )(payload: Http4sPayload[P], base: Uri, client: Http4sClient[F]): A => F[Either[E | D, B]] =
      new Http4s.ClientBuilder[F, A, Either[E | D, B]]
        .apply(endpoint.compose(api.errors).client)(payload, base, client)

    def apply[P[-_, +_], E](
        endpoint: Endpoint.WithErrors[Http4sPayload.Supported[P], A, Any, Nothing, B, E]
    )(payload: Http4sPayload[P], base: Uri, client: Http4sClient[F]): A => F[Either[E | Status, B]] =
      new Http4s.ClientBuilder[F, A, Either[E | Status, B]]
        .apply(endpoint.compose(ErrorPolicy.default).client)(payload, base, client)

    def apply[P[-_, +_]](
        endpoint: Endpoint.Client[Http4sPayload.Supported[P], A, B]
    )(payload: Http4sPayload[P], base: Uri, client: Http4sClient[F]): A => F[B] =
      val encoder = Http4sRequestEncoder(payload)
      val decoder = Http4sResponseDecoder(payload)

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
            .decode(endpoint.responses, Http4sWire.Response(Status(response._1), response._2._1, response._2._2))
            .leftMap(Http4sFailure.Response.apply)
            .liftTo[F]
        yield decoded

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
    Http4sEnvelope
      .toHttp4sResponse[F](response)
      .leftMap(failure => Http4sFailure.Status(response.status, failure.message))

  private[http] def respond[F[_]](
      response: Either[Http4sIssue, Http4sWire.Response]
  )(using F: Concurrent[F]): F[Http4sResponse[F]] =
    response.leftMap(Http4sFailure.Encoding.apply).flatMap(Http4s.respondable[F]).liftTo[F]

  private def raise[F[_], A](value: Either[Http4sIssue, A])(using F: Concurrent[F]): F[A] =
    value.leftMap(Http4sFailure.Encoding.apply).liftTo[F]

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
