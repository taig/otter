package io.taig.otter.http

import cats.data.Chain
import cats.effect.Concurrent
import cats.effect.Resource
import cats.syntax.all.*
import io.taig.otter.http.codec.Http4sInterpreter
import io.taig.otter.http.codec.Http4sRequestDecoder
import io.taig.otter.http.codec.Http4sResponseEncoder
import io.taig.otter.http.codec.PathTemplate
import io.taig.otter.http.component.ErrorPolicyComponent
import org.http4s.Entity
import org.http4s.Request as Http4sRequest
import org.http4s.Response as Http4sResponse

/** An endpoint, and what answers it.
  *
  * `A => F[B]` and nothing wider is the whole of what a handler is, because the endpoint has already said what a
  * request holds and what an answer may be. There is no request object to reach into and no response builder to get
  * wrong: a status is chosen by which branch of the response union the value took, and a handler that returns the wrong
  * shape does not compile.
  *
  * The constructor is private, and the companion has exactly one `apply` taking two arguments, because a second one
  * costs the handler its parameter types. Scala types a lambda against its expected type only once overloading has
  * settled on one alternative, so with two candidates of the same arity `(filter, _) => ...` has nothing to untuple
  * against and a handler for a composed request is left reaching for `input._1`. A case class constructor is such a
  * candidate wherever it is visible, and `private[http]` would still have offered it to every route written inside this
  * package. Every [[Endpoint.Declaration]] goes through the one `apply`, a [[ComposedEndpoint]] through
  * [[Route.composed]], and only the `api` form, which differs in arity, keeps the name.
  *
  * A route keeps the [[Endpoint.Declaration]] it was built from, so what is served can be rendered as a document by
  * handing [[declaration]] to a renderer, and the handler is all that is thrown away. A composed endpoint is kept as
  * [[ComposedEndpoint.declaration]], which overrides every entry of its policy: it answers with that policy under an
  * API too, and a document rendered from it says so. Ordinary declarations instead inherit the serving API's policy,
  * with their local overrides applied on top. The API passed to `Route(api, endpoint, handler)` supplies the initial
  * defaults for standalone serving; serving that route under another API replaces those defaults.
  *
  * Documentation membership is explicit: include `route.declaration` or `routes.declarations` in the documented API to
  * describe the declarations being served. Constructing or serving a route does not add it to an API.
  */
final case class Route[F[_], +S[-_, +_], A, B] private (
    declaration: Endpoint.Declaration[S, S, Nothing, A, B, Any, Any],
    handler: A => F[B],
    errors: ErrorPolicy[S, Any]
):
  /** The endpoint as the server sees it, without its error declarations. */
  def endpoint: Endpoint.Server[S, A, B] = declaration.domain

  /** This route under an API's policy, with its own overrides applied on top of it.
    *
    * A method rather than a `copy`, because a private constructor takes `copy` with it.
    */
  private[http] def under[T[-w, +r] >: S[w, r]](policy: ErrorPolicy[T, Any]): Route[F, T, A, B] =
    new Route(declaration, handler, declaration.overrides(policy))

  /** Whether this route is the one an incoming method and path is addressed to. */
  def matches(method: Method, segments: Vector[String]): Boolean =
    endpoint.request.method == method && addresses(segments)

  /** Whether this route spells an incoming path, whatever method it arrived under.
    *
    * Arity and literals, and deliberately nothing else. [[io.taig.otter.http.codec.PathDecoder]] would answer a
    * stricter question and answer it in one piece -- a tuple decoder rejects the wrong number of segments, a `Constant`
    * rejects a mis-spelled literal, and a parameter that will not parse fails alongside both -- so a router that asked
    * it could not tell "this is some other endpoint" from "this endpoint, called wrongly". The first must fall through
    * to the next route and end as a `404`, or as a `405` where another route spells the path; the second must stop here
    * and be reported as a `400`. Deciding on the part of a path that cannot vary is what separates them.
    */
  def addresses(segments: Vector[String]): Boolean =
    Route.addresses(PathTemplate(endpoint.request.path.value), segments)

object Route:
  /** Build a route with this API's initial defaults and the declaration's local overrides.
    *
    * Standalone serving uses this policy. Serving under another API replaces the inherited defaults while keeping the
    * declaration's overrides. Use [[Route.composed]] with `endpoint.compose(api.errors)` to retain the complete policy
    * under any later API. The route keeps the original declaration, and does not register it in either API's documents.
    */
  def apply[F[_], Q[-_, +_], S[-_, +_], T[-_, +_], A, B, E, D](
      api: Api[T, E],
      endpoint: Endpoint.Declaration[Q, S, Nothing, A, B, Any, D],
      handler: A => F[B]
  ): Route[F, [w, r] =>> Q[w, r] | S[w, r] | T[w, r], A, B] =
    new Route(endpoint, handler, endpoint.compose[Body.Or[S, T], E](api.errors).errors)

  def apply[F[_], Q[-_, +_], S[-_, +_], A, B, E](
      endpoint: Endpoint.Declaration[Q, S, Nothing, A, B, Any, E],
      handler: A => F[B]
  ): Route[F, Body.Or[Q, S], A, B] =
    new Route(endpoint, handler, endpoint.compose(ErrorPolicyComponent.default).errors)

  /** Retain every category of an already-composed policy, including when served under another API. */
  def composed[F[_], Q[-_, +_], S[-_, +_], A, B, E](
      endpoint: ComposedEndpoint[Q, S, Nothing, A, B, Any, E],
      handler: A => F[B]
  ): Route[F, Body.Or[Q, S], A, B] = new Route(endpoint.declaration, handler, endpoint.errors)

  /** This route's answer to a request it has already matched.
    *
    * A function of the route rather than a method on it, because the codecs are written at the requirement the
    * interpreter covers and a [[Route]] is covariant in its own. Naming the route here, at `Supported[P]`, is what puts
    * the two in the same scope: the endpoint's request, its responses and its error policy all widen to it, so the
    * schemas reach the codecs already typed and neither side has to be told what the other holds.
    */
  private[http] def run[F[_], P[-_, +_], Q[-_, +_], A, B](
      route: Route[F, Http4sInterpreter.Supported[F, P, Q], A, B],
      decoder: Http4sRequestDecoder[F, P, Q],
      encoder: Http4sResponseEncoder[F, P, Q],
      observe: Http4sObservation[F] => F[Unit],
      request: Http4sRequest[F],
      segments: Vector[String]
  )(using F: Concurrent[F]): F[Http4sResponse[F]] =
    def evaluate[T](value: => T): F[T] = F.unit.flatMap(_ => F.catchNonFatal(value))
    def notify(event: Http4sObservation.Event): F[Unit] =
      evaluate(observe(Http4sObservation(request, route.endpoint, event))).flatten
    def failure(cause: Throwable): Failure = cause match
      case Http4sFailure.Execution(refused)   => refused
      case streaming: Http4sFailure.Streaming => streaming.failure
      case _: Http4sFailure.Encoding          => Failure(Failure.Category.Encoding, cause = Some(cause))
      case _: Http4sFailure.Status            => Failure(Failure.Category.Status, cause = Some(cause))
      case _                                  => Failure(Failure.Category.Unexpected, cause = Some(cause))

    def observed(response: Http4sResponse[F], errorResponse: Boolean): Http4sResponse[F] = response.entity match
      case Entity.Streamed(stream, length) =>
        val contextual =
          Http4sStreaming.context(route.endpoint, Http4sFailure.Direction.Response, Failure.Category.Encoding)(stream)
        response.withEntity(
          Entity.Streamed(
            contextual.onFinalizeCase {
              case Resource.ExitCase.Errored(cause) =>
                notify(if errorResponse then Http4sObservation.Event.ErrorResponseFailed(cause)
                else Http4sObservation.Event.Failed(failure(cause)))
              case Resource.ExitCase.Canceled  => notify(Http4sObservation.Event.Cancelled)
              case Resource.ExitCase.Succeeded => F.unit
            },
            length
          )
        )
      case _ => response

    val wire = Http4sWire.Request[F](
      segments,
      Http4sEnvelope.toQueries(request.uri.query),
      Http4sEnvelope.toHeaders(request.headers),
      (Http4sEnvelope.toMediaType(request.headers), request.entity)
    )
    val decoded = decoder
      .contextual(route.endpoint)
      .resource(route.endpoint.request, wire)
      .handleErrorWith(cause =>
        Resource.eval(F.raiseError(Http4sFailure.Execution(Failure(Failure.Category.EntityRead, cause = Some(cause)))))
      )
    val execute = F.uncancelable: poll =>
      poll(decoded.evalMap {
        case cats.data.Validated.Invalid(refused) => F.pure(Left(refused.failure))
        case cats.data.Validated.Valid(value)     =>
          evaluate(route.handler(value)).flatten
            .flatMap(value =>
              evaluate(encoder.encode(route.endpoint.responses, value)).flatten.handleErrorWith(cause =>
                F.raiseError(Http4sFailure.Execution(Failure(Failure.Category.Encoding, cause = Some(cause))))
              )
            )
            .flatMap(Http4s.respond[F])
            .map(response => Right(observed(response, false)))
      }.allocated).flatMap:
        case (Right(response), release) =>
          response.entity match
            case Entity.Streamed(stream, length) =>
              F.pure(Right(response.withEntity(Entity.Streamed(stream.onFinalize(release), length))))
            case _ => release.as(Right(response))
        case (Left(refused), release) => release.as(Left(refused))

    val respond = execute
      .handleError(cause => Left(failure(cause)))
      .flatMap:
        case Right(response) => F.pure(response)
        case Left(refused)   =>
          val render = evaluate(encoder.encode(route.errors.responses, refused)).flatten
            .flatMap(Http4s.respond[F])
            .map(observed(_, true))
            .handleErrorWith: cause =>
              notify(Http4sObservation.Event.ErrorResponseFailed(cause)).attempt *> F.raiseError(cause)
          notify(Http4sObservation.Event.Failed(refused)) *> render

    F.onCancel(respond, notify(Http4sObservation.Event.Cancelled))

  /** Whether a path template spells an incoming path, which is [[Route.addresses]] for a template already taken apart.
    */
  private[http] def addresses(
      template: Chain[Either[String, (String, Parameter.Node[?, ?])]],
      segments: Vector[String]
  ): Boolean =
    template.toList.corresponds(segments):
      case (Left(literal), segment) => literal == segment
      case (Right(_), _)            => true
