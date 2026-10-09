package io.taig.otter.http

import cats.data.Chain
import cats.data.OptionT
import cats.effect.Concurrent
import cats.effect.Resource
import cats.syntax.all.*
import io.taig.otter.Step
import io.taig.otter.Violations
import io.taig.otter.http.codec.Http4sInterpreter
import io.taig.otter.http.codec.Http4sPayload
import io.taig.otter.http.codec.Http4sRequestDecoder
import io.taig.otter.http.codec.Http4sRequestEncoder
import io.taig.otter.http.codec.Http4sResponseDecoder
import io.taig.otter.http.codec.Http4sResponseEncoder
import io.taig.otter.http.component.ErrorPolicyComponent
import io.taig.otter.http.component.UnroutedPolicyComponent
import org.http4s.HttpApp
import org.http4s.HttpRoutes
import org.http4s.Request as Http4sRequest
import org.http4s.Response as Http4sResponse
import org.http4s.Uri
import org.http4s.client.Client as Http4sClient

import java.util.Locale

/** Endpoints, served and called.
  *
  * The bridge object, on the pattern [[io.taig.otter.JsonCirce]] and [[io.taig.otter.JsonBorer]] set: the codecs are
  * the interpreter, and this is where they become the thing a caller actually wanted -- an `HttpRoutes`, an `HttpApp`,
  * a function -- and where failures become the responses declared by each route's error policy, and a request no route
  * matched the response declared by the API's unrouted policy.
  */
object Http4s:
  /** A set of routes, tried in the order they are given.
    *
    * `HttpRoutes`, falling through on a request none of these describe, because that is what lets them be composed with
    * `<+>` beside routes Otter knows nothing about. [[Http4s.app]] is where falling through stops and becomes an
    * answer, and [[Http4s.fallback]] is that answer on its own for when something else is mounted beside these.
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
    def apply[P[-_, +_], Q[-_, +_]](routes: Route[F, Http4sInterpreter.Supported[F, P, Q], ?, ?]*)(
        payload: Http4sInterpreter.Of[P, Q]
    ): HttpRoutes[F] = apply(Routes(routes*))(payload)

    def apply[P[-_, +_], Q[-_, +_]](routes: Route[F, Http4sInterpreter.Supported[F, P, Q], ?, ?]*)(
        payload: Http4sInterpreter.Of[P, Q],
        observe: Http4sObservation[F] => F[Unit]
    ): HttpRoutes[F] = apply(Routes(routes*))(payload, observe)

    /** Apply the API defaults and each route's endpoint-local overrides. */
    def apply[P[-_, +_], Q[-_, +_], S[-w, +r] <: Http4sInterpreter.Supported[F, P, Q][w, r], E](
        api: Api[S, E],
        routes: Route[F, Http4sInterpreter.Supported[F, P, Q], ?, ?]*
    )(payload: Http4sInterpreter.Of[P, Q]): HttpRoutes[F] = apply(Http4s.composed(api, routes))(payload)

    def apply[P[-_, +_], Q[-_, +_], S[-w, +r] <: Http4sInterpreter.Supported[F, P, Q][w, r], E](
        api: Api[S, E],
        routes: Route[F, Http4sInterpreter.Supported[F, P, Q], ?, ?]*
    )(payload: Http4sInterpreter.Of[P, Q], observe: Http4sObservation[F] => F[Unit]): HttpRoutes[F] =
      apply(Http4s.composed(api, routes))(payload, observe)

    def apply[P[-_, +_], Q[-_, +_]](routes: Routes[F, Http4sInterpreter.Supported[F, P, Q]])(
        payload: Http4sInterpreter.Of[P, Q]
    ): HttpRoutes[F] = apply(routes)(payload, (_: Http4sObservation[F]) => Concurrent[F].unit)

    def apply[P[-_, +_], Q[-_, +_]](routes: Routes[F, Http4sInterpreter.Supported[F, P, Q]])(
        payload: Http4sInterpreter.Of[P, Q],
        observe: Http4sObservation[F] => F[Unit]
    ): HttpRoutes[F] =
      routes.values.toList.foreach(route => Http4sStreaming.validate(route.declaration.compose(route.errors).effective))
      val decoder = Http4sRequestDecoder[F, P, Q](payload)
      val encoder = Http4sResponseEncoder[F, P, Q](payload)

      HttpRoutes[F]: request =>
        val method = Http4sEnvelope.toMethod(request.method)
        val segments = Http4sEnvelope.toPath(request.uri.path)

        OptionT
          .fromOption[F](routes.values.find(_.matches(method, segments)))
          .semiflatMap(Route.run(_, decoder, encoder, observe, request, segments))

  /** A set of routes, tried in the order they are given, and answering every request none of them matched.
    *
    * The answer is one only the router can give. A path some route spells under other methods is a `405` carrying
    * `Allow`, and any other is a `404`; after falling through, nothing could tell the two apart or name the methods.
    * Both are written through the API's [[UnroutedPolicy]], and without an API through
    * [[UnroutedPolicyComponent.default]], exactly as a route without one is answered by
    * [[ErrorPolicyComponent.default]].
    *
    * `Allow` is written by the interpreter whenever the declared answer is a `405`, and replaces any the declaration
    * wrote, because only the router knows the set. It lists what is routed, never what is merely documented.
    *
    * Deliberately out of scope: `HEAD` is not synthesised from `GET`, so on a `GET` path it is a `405` with
    * `Allow: GET`; `OPTIONS` is not answered; and a method no route serves anywhere is, on a path some route spells, a
    * `405` rather than a `501`. `DefaultHead` retries a `HEAD` as a `GET` only when what it wraps falls through, so it
    * has to wrap [[Http4s.routes]] with [[Http4s.fallback]] composed after it, and cannot wrap this. CORS wraps either.
    *
    * `observe` sees what the routes see and nothing more: a request no route matched has no endpoint to be observed
    * against, and failing to write its answer is raised in `F`.
    */
  def app[F[_]: Concurrent]: Http4s.AppBuilder[F] = new Http4s.AppBuilder[F]

  final class AppBuilder[F[_]: Concurrent]:
    def apply[P[-_, +_], Q[-_, +_]](routes: Route[F, Http4sInterpreter.Supported[F, P, Q], ?, ?]*)(
        payload: Http4sInterpreter.Of[P, Q]
    ): HttpApp[F] = apply(Routes(routes*))(payload)

    def apply[P[-_, +_], Q[-_, +_]](routes: Route[F, Http4sInterpreter.Supported[F, P, Q], ?, ?]*)(
        payload: Http4sInterpreter.Of[P, Q],
        observe: Http4sObservation[F] => F[Unit]
    ): HttpApp[F] = apply(Routes(routes*))(payload, observe)

    /** Apply the API defaults and each route's endpoint-local overrides, and answer unrouted requests as it declares.
      */
    def apply[P[-_, +_], Q[-_, +_], S[-w, +r] <: Http4sInterpreter.Supported[F, P, Q][w, r], E](
        api: Api[S, E],
        routes: Route[F, Http4sInterpreter.Supported[F, P, Q], ?, ?]*
    )(payload: Http4sInterpreter.Of[P, Q]): HttpApp[F] =
      Http4s.terminal(Http4s.composed(api, routes), api.unrouted)(
        payload,
        (_: Http4sObservation[F]) => Concurrent[F].unit
      )

    def apply[P[-_, +_], Q[-_, +_], S[-w, +r] <: Http4sInterpreter.Supported[F, P, Q][w, r], E](
        api: Api[S, E],
        routes: Route[F, Http4sInterpreter.Supported[F, P, Q], ?, ?]*
    )(payload: Http4sInterpreter.Of[P, Q], observe: Http4sObservation[F] => F[Unit]): HttpApp[F] =
      Http4s.terminal(Http4s.composed(api, routes), api.unrouted)(payload, observe)

    def apply[P[-_, +_], Q[-_, +_]](routes: Routes[F, Http4sInterpreter.Supported[F, P, Q]])(
        payload: Http4sInterpreter.Of[P, Q]
    ): HttpApp[F] = apply(routes)(payload, (_: Http4sObservation[F]) => Concurrent[F].unit)

    def apply[P[-_, +_], Q[-_, +_]](routes: Routes[F, Http4sInterpreter.Supported[F, P, Q]])(
        payload: Http4sInterpreter.Of[P, Q],
        observe: Http4sObservation[F] => F[Unit]
    ): HttpApp[F] = Http4s.terminal(routes, UnroutedPolicyComponent.default)(payload, observe)

  /** The answer [[Http4s.app]] gives a request its routes did not match, as `HttpRoutes` that never fall through.
    *
    * For when something Otter knows nothing about is mounted beside the routes:
    * `(health <+> Http4s.routes[F](api, served*)(payload) <+> Http4s.fallback[F](api, served*)(payload)).orNotFound`.
    * It asks only the routes it is given, so it has to be given the same ones that were tried ahead of it: a path only
    * a foreign route serves is a `404` here whatever its method, and a foreign route's methods never reach `Allow`.
    * With an API, answers come from `api.unrouted`; endpoint handlers and their error policies are never executed.
    */
  def fallback[F[_]: Concurrent]: Http4s.FallbackBuilder[F] = new Http4s.FallbackBuilder[F]

  final class FallbackBuilder[F[_]: Concurrent]:
    def apply[P[-_, +_], Q[-_, +_]](routes: Route[F, Http4sInterpreter.Supported[F, P, Q], ?, ?]*)(
        payload: Http4sInterpreter.Of[P, Q]
    ): HttpRoutes[F] = apply(Routes(routes*))(payload)

    def apply[P[-_, +_], Q[-_, +_], S[-w, +r] <: Http4sInterpreter.Supported[F, P, Q][w, r], E](
        api: Api[S, E],
        routes: Route[F, Http4sInterpreter.Supported[F, P, Q], ?, ?]*
    )(payload: Http4sInterpreter.Of[P, Q]): HttpRoutes[F] =
      val answer = Http4s.unroutedAnswer(Routes(routes*), api.unrouted, payload)
      HttpRoutes[F](request => OptionT.liftF(answer(request)))

    def apply[P[-_, +_], Q[-_, +_]](routes: Routes[F, Http4sInterpreter.Supported[F, P, Q]])(
        payload: Http4sInterpreter.Of[P, Q]
    ): HttpRoutes[F] =
      val answer = Http4s.unroutedAnswer(routes, UnroutedPolicyComponent.default, payload)
      HttpRoutes[F](request => OptionT.liftF(answer(request)))

  private def terminal[F[_]: Concurrent, P[-_, +_], Q[-_, +_]](
      routes: Routes[F, Http4sInterpreter.Supported[F, P, Q]],
      unrouted: UnroutedPolicy[Http4sInterpreter.Supported[F, P, Q]]
  )(payload: Http4sInterpreter.Of[P, Q], observe: Http4sObservation[F] => F[Unit]): HttpApp[F] =
    val served = Http4s.routes[F](routes)(payload, observe)
    val answer = Http4s.unroutedAnswer(routes, unrouted, payload)
    HttpApp[F](request => served.run(request).getOrElseF(answer(request)))

  /** The declared answer to a request none of `routes` matched, with the `Allow` header only the router can write. */
  private def unroutedAnswer[F[_], P[-_, +_], Q[-_, +_]](
      routes: Routes[F, Http4sInterpreter.Supported[F, P, Q]],
      policy: UnroutedPolicy[Http4sInterpreter.Supported[F, P, Q]],
      payload: Http4sInterpreter.Of[P, Q]
  )(using F: Concurrent[F]): Http4sRequest[F] => F[Http4sResponse[F]] =
    val encoder = Http4sResponseEncoder[F, P, Q](payload)

    request =>
      F.unit
        .flatMap: _ =>
          F.catchNonFatal:
            val unrouted =
              routes.unrouted(Http4sEnvelope.toMethod(request.method), Http4sEnvelope.toPath(request.uri.path))
            encoder.encode(policy.responses, unrouted).map(_.map(Http4s.allow(unrouted, _)))
        .flatten
        .flatMap(Http4s.respond[F])

  /** Every `405` carries `Allow`, and an empty one where the declaration answered a path no route spells with a `405`,
    * which is what the specification says a resource allowing no methods sends.
    */
  private def allow[F[_]](unrouted: Unrouted, response: Http4sWire.Response[F]): Http4sWire.Response[F] =
    if response.status.value != 405 then response
    else
      val allowed = unrouted match
        case Unrouted.MethodNotAllowed(_, _, allowed) => allowed.toChain.toList.map(_.name)
        case _: Unrouted.NotFound                     => Nil
      val headers = response.headers.filterNot((name, _) => name.toLowerCase(Locale.ROOT) == "allow")
      response.copy(headers = headers :+ ("Allow", allowed.mkString(", ")))

  /** Apply the serving API's defaults beneath each declaration's local overrides.
    *
    * This replaces the initial defaults of `Route(otherApi, endpoint, handler)`. A [[Route.composed]] declaration
    * overrides every category, so its complete policy survives. Documentation membership remains the caller's choice:
    * include these routes' declarations in the API to render the same contracts that are served.
    */
  private def composed[F[_], P[-_, +_], Q[-_, +_], S[-w, +r] <: Http4sInterpreter.Supported[F, P, Q][w, r], E](
      api: Api[S, E],
      routes: Seq[Route[F, Http4sInterpreter.Supported[F, P, Q], ?, ?]]
  ): Routes[F, Http4sInterpreter.Supported[F, P, Q]] =
    Routes(routes.map(_.under(api.errors))*)

  /** Bind the transport context once, then derive a typed function from each endpoint declaration.
    *
    * The interpreter fixes the supported payload alphabet at configuration time. Each endpoint is checked against that
    * capability when selected, with its request writer and response reader determining the function's types.
    * Construction performs no requests; the caller owns the underlying transport's lifetime.
    */
  def client[F[_]: Concurrent, P[-_, +_], Q[-_, +_]](
      payload: Http4sInterpreter.Of[P, Q],
      base: Uri,
      transport: Http4sClient[F]
  ): Http4s.Client[F, P, Q] = new Http4s.Client(payload, base, transport)

  final class Client[F[_]: Concurrent, P[-_, +_], Q[-_, +_]] private[Http4s] (
      payload: Http4sInterpreter.Of[P, Q],
      base: Uri,
      transport: Http4sClient[F]
  ):
    private val encoder = Http4sRequestEncoder[F, P, Q](payload)
    private val decoder = Http4sResponseDecoder[F, P, Q](payload)

    def withApi[A <: Api[?, ?]](api: A)(using
        ErrorPolicy[api.Requirement, api.Error] <:< ErrorPolicy[Http4sInterpreter.Supported[F, P, Q], api.Error],
        UnroutedPolicy[api.Requirement] <:< UnroutedPolicy[Http4sInterpreter.Supported[F, P, Q]]
    ): Http4s.ApiClient[F, P, Q, api.Requirement, api.Error] =
      new Http4s.ApiClient(this, api.typed)

    def apply[A, B, D](
        endpoint: Endpoint.WithErrors[Http4sInterpreter.Supported[F, P, Q], Http4sPayload.Supported[
          P
        ], A, Any, Nothing, B, D]
    ): A => F[Either[Status | D, B]] = apply(endpoint.compose(ErrorPolicyComponent.default).client)

    def apply[A, B](
        endpoint: Endpoint.Schema[Http4sInterpreter.Supported[F, P, Q], Http4sPayload.Supported[P], A, Any, Nothing, B]
    ): A => F[B] =
      val call = resource(endpoint)
      value => call(value).use(_.pure[F])

    def resource[A, B, D](
        endpoint: Endpoint.WithErrors[
          Http4sInterpreter.Supported[F, P, Q],
          Http4sInterpreter.Supported[F, P, Q],
          A,
          Any,
          Nothing,
          B,
          D
        ]
    ): A => Resource[F, Either[Status | D, B]] = resource(endpoint.compose(ErrorPolicyComponent.default).client)

    def resource[A, B](endpoint: Endpoint.Client[Http4sInterpreter.Supported[F, P, Q], A, B]): A => Resource[F, B] =
      Http4sStreaming.validate(endpoint)
      value =>
        for
          wire <- Resource.eval(encoder.encode(endpoint.request, value).flatMap(Http4s.raise[F, Http4sWire.Request[F]]))
          method <- Resource.eval(
            Http4sEnvelope
              .toHttp4sMethod(endpoint.request.method)
              .leftMap(failure => Http4sFailure.Method(endpoint.request.method, failure.message))
              .liftTo[F]
          )
          request = Http4s.toHttp4sRequest[F](method, base, wire)
          response <- transport.run(
            request.withBodyStream(
              Http4sStreaming.context(endpoint, Http4sFailure.Direction.Request, Failure.Category.Encoding)(
                request.body
              )
            )
          )
          decoded <- Resource.eval(
            decoder
              .contextual(endpoint)
              .decode(endpoint.responses, Http4s.toWire(response))
              .flatMap(_.leftMap(Http4sFailure.Response.apply).liftTo[F])
          )
        yield decoded

  final class ApiClient[F[_], P[-_, +_], Q[-_, +_], S[-_, +_], E] private[Http4s] (
      client: Http4s.Client[F, P, Q],
      api: Api[S, E]
  )(using supported: ErrorPolicy[S, E] <:< ErrorPolicy[Http4sInterpreter.Supported[F, P, Q], E]):
    def apply[A, B, D](
        endpoint: Endpoint.Declaration[Http4sInterpreter.Supported[F, P, Q], Http4sPayload.Supported[
          P
        ], A, Any, Nothing, B, D]
    )(using buffered: ErrorPolicy[S, E] <:< ErrorPolicy[Http4sPayload.Supported[P], E]): A => F[Either[E | D, B]] =
      client(endpoint.compose(buffered(api.errors)).client)

    def resource[A, B, D](
        endpoint: Endpoint.Declaration[
          Http4sInterpreter.Supported[F, P, Q],
          Http4sInterpreter.Supported[F, P, Q],
          A,
          Any,
          Nothing,
          B,
          D
        ]
    ): A => Resource[F, Either[E | D, B]] = client.resource(endpoint.compose(supported(api.errors)).client)

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

  private def respondable[F[_]](response: Http4sWire.Response[F]): Either[Http4sFailure, Http4sResponse[F]] =
    Http4sEnvelope
      .toHttp4sResponse[F](response)
      .leftMap(failure => Http4sFailure.Status(response.status, failure.message))

  private[http] def respond[F[_]](
      response: Either[Http4sIssue, Http4sWire.Response[F]]
  )(using F: Concurrent[F]): F[Http4sResponse[F]] =
    response.leftMap(Http4sFailure.Encoding.apply).flatMap(Http4s.respondable[F]).liftTo[F]

  private def raise[F[_], A](value: Either[Http4sIssue, A])(using F: Concurrent[F]): F[A] =
    value.leftMap(Http4sFailure.Encoding.apply).liftTo[F]

  private def toHttp4sRequest[F[_]](
      method: org.http4s.Method,
      base: Uri,
      wire: Http4sWire.Request[F]
  ): Http4sRequest[F] =
    val path = wire.path.foldLeft(base.path.toAbsolute)(_.addSegment(_))
    val headers = wire.headers ++ Chain.fromOption(wire.body._1.map(mediaType => ("Content-Type", mediaType.render)))

    Http4sRequest[F](
      method = method,
      uri = base.withPath(path).copy(query = Http4sEnvelope.toHttp4sQuery(wire.queries)),
      headers = Http4sEnvelope.toHttp4sHeaders(headers),
      entity = wire.body._2
    )

  private def toWire[F[_]](response: Http4sResponse[F]): Http4sWire.Response[F] =
    Http4sWire.Response(
      Status(response.status.code),
      Http4sEnvelope.toHeaders(response.headers),
      (Http4sEnvelope.toMediaType(response.headers), response.entity)
    )
