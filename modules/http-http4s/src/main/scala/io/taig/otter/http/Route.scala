package io.taig.otter.http

import cats.data.Chain
import cats.effect.Concurrent
import cats.syntax.all.*
import io.taig.otter.http.codec.Http4sPayload
import io.taig.otter.http.codec.Http4sRequestDecoder
import io.taig.otter.http.codec.Http4sResponseEncoder
import io.taig.otter.http.codec.PathTemplate
import org.http4s.Request as Http4sRequest
import org.http4s.Response as Http4sResponse
import scodec.bits.ByteVector

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
  * API too, and a document rendered from it says so.
  */
final case class Route[F[_], +S[-_, +_], A, B] private (
    declaration: Endpoint.Declaration[S, Nothing, A, B, Any, Any],
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
  def apply[F[_], S[-_, +_], A, B, E, D](
      api: Api[S, E],
      endpoint: Endpoint.Declaration[S, Nothing, A, B, Any, D],
      handler: A => F[B]
  ): Route[F, S, A, B] =
    new Route(endpoint, handler, endpoint.compose(api.errors).errors)

  def apply[F[_], S[-_, +_], A, B, E](
      endpoint: Endpoint.Declaration[S, Nothing, A, B, Any, E],
      handler: A => F[B]
  ): Route[F, S, A, B] =
    new Route(endpoint, handler, endpoint.compose(ErrorPolicy.default).errors)

  /** A route for an endpoint already composed with its error policy, which it keeps under an API's policy as well.
    *
    * A name of its own rather than an overload of `apply`, for the reason [[Route]] gives.
    */
  def composed[F[_], S[-_, +_], A, B, E](
      endpoint: ComposedEndpoint[S, Nothing, A, B, Any, E],
      handler: A => F[B]
  ): Route[F, S, A, B] = new Route(endpoint.declaration, handler, endpoint.errors)

  /** This route's answer to a request it has already matched.
    *
    * A function of the route rather than a method on it, because the codecs are written at the requirement the
    * interpreter covers and a [[Route]] is covariant in its own. Naming the route here, at `Supported[P]`, is what puts
    * the two in the same scope: the endpoint's request, its responses and its error policy all widen to it, so the
    * schemas reach the codecs already typed and neither side has to be told what the other holds.
    */
  private[http] def run[F[_], P[-_, +_], A, B](
      route: Route[F, Http4sPayload.Supported[P], A, B],
      decoder: Http4sRequestDecoder[P],
      encoder: Http4sResponseEncoder[P],
      observe: Http4sObservation[F] => F[Unit],
      request: Http4sRequest[F],
      segments: Vector[String]
  )(using F: Concurrent[F]): F[Http4sResponse[F]] =
    def evaluate[T](value: => T): F[T] = F.unit.flatMap(_ => F.catchNonFatal(value))
    def notify(event: Http4sObservation.Event): F[Unit] =
      evaluate(observe(Http4sObservation(request, route.endpoint, event))).flatten
    def failure(cause: Throwable): Failure = cause match
      case Http4sFailure.Execution(refused) => refused
      case _: Http4sFailure.Encoding        => Failure(Failure.Category.Encoding, cause = Some(cause))
      case _: Http4sFailure.Status          => Failure(Failure.Category.Status, cause = Some(cause))
      case _                                => Failure(Failure.Category.Unexpected, cause = Some(cause))

    val execute = evaluate(Route.bytes(route.endpoint, request)).flatten.attempt.flatMap:
      case Left(cause)  => F.pure(Left(Failure(Failure.Category.EntityRead, cause = Some(cause))))
      case Right(bytes) =>
        evaluate:
          val wire = Http4sWire.Request(
            path = segments,
            queries = Http4sEnvelope.toQueries(request.uri.query),
            headers = Http4sEnvelope.toHeaders(request.headers),
            body = (Http4sEnvelope.toMediaType(request.headers), bytes)
          )
          decoder.decodeDetailed(route.endpoint.request, wire)
        .flatMap:
          case cats.data.Validated.Invalid(refused) => F.pure(Left(refused.failure))
          case cats.data.Validated.Valid(value)     =>
            evaluate(route.handler(value)).flatten
              .flatMap(value =>
                evaluate(encoder.encode(route.endpoint.responses, value)).handleErrorWith(cause =>
                  F.raiseError(Http4sFailure.Execution(Failure(Failure.Category.Encoding, cause = Some(cause))))
                )
              )
              .flatMap(Http4s.respond[F])
              .map(Right(_))

    val respond = execute
      .handleError(cause => Left(failure(cause)))
      .flatMap:
        case Right(response) => F.pure(response)
        case Left(refused)   =>
          val render = evaluate(encoder.encode(route.errors.responses, refused))
            .flatMap(Http4s.respond[F])
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

  /** The request's bytes, read only if the endpoint describes something to read them as.
    *
    * An endpoint with no body never touches the entity at all, which is what keeps a `GET` from paying for a stream it
    * was never going to look at.
    */
  private def bytes[F[_]: Concurrent, S[-_, +_]](
      endpoint: Endpoint.Server[S, ?, ?],
      request: Http4sRequest[F]
  ): F[ByteVector] =
    if endpoint.request.bodies.isEmpty then ByteVector.empty.pure else Http4sEnvelope.toBytes(request.entity)
