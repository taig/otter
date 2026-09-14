package io.taig.otter.http

import cats.effect.Concurrent
import cats.syntax.all.*
import io.taig.otter.http.codec.Http4sRequestDecoderUnchecked
import io.taig.otter.http.codec.Http4sResponseEncoderUnchecked
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
  */
final case class Route[F[_], +S[-_, +_], A, B](
    endpoint: Endpoint.Server[S, A, B],
    handler: A => F[B],
    errors: ErrorPolicy[S, Any] = ErrorPolicy.default,
    overrides: ErrorOverrides[S, Any] = ErrorOverrides()
):
  /** Whether this route is the one an incoming method and path is addressed to.
    *
    * Arity and literals, and deliberately nothing else. [[io.taig.otter.http.codec.PathDecoder]] would answer a
    * stricter question and answer it in one piece -- a tuple decoder rejects the wrong number of segments, a `Constant`
    * rejects a mis-spelled literal, and a parameter that will not parse fails alongside both -- so a router that asked
    * it could not tell "this is some other endpoint" from "this endpoint, called wrongly". The first must fall through
    * to the next route and end as a `404`; the second must stop here and be reported as a `400`. Deciding on the part
    * of a path that cannot vary is what separates them.
    */
  def matches(method: Method, segments: Vector[String]): Boolean =
    endpoint.request.method == method &&
      PathTemplate(endpoint.request.path.value).toList.corresponds(segments):
        case (Left(literal), segment) => literal == segment
        case (Right(_), _)            => true

  /** This route's answer to a request it has already matched. */
  private[http] def run(
      decoder: Http4sRequestDecoderUnchecked,
      encoder: Http4sResponseEncoderUnchecked,
      observe: Http4sObservation[F] => F[Unit],
      request: Http4sRequest[F],
      segments: Vector[String]
  )(using F: Concurrent[F]): F[Http4sResponse[F]] =
    def evaluate[T](value: => T): F[T] = F.unit.flatMap(_ => F.catchNonFatal(value))
    def notify(event: Http4sObservation.Event): F[Unit] =
      evaluate(observe(Http4sObservation(request, endpoint, event))).flatten
    def failure(cause: Throwable): Failure = cause match
      case Http4sFailure.Execution(refused)                   => refused
      case Http4sFailure.Interpreter(_: Http4sIssue.Encoding) => Failure(Failure.Category.Encoding, cause = Some(cause))
      case Http4sFailure.Interpreter(_) => Failure(Failure.Category.Interpreter, cause = Some(cause))
      case _: Http4sFailure.Status      => Failure(Failure.Category.Status, cause = Some(cause))
      case _                            => Failure(Failure.Category.Unexpected, cause = Some(cause))

    val execute = evaluate(Route.bytes(endpoint, request)).flatten.attempt.flatMap:
      case Left(cause)  => F.pure(Left(Failure(Failure.Category.EntityRead, cause = Some(cause))))
      case Right(bytes) =>
        evaluate:
          val wire = Http4sWire.Request(
            path = segments,
            queries = Http4sEnvelope.toQueries(request.uri.query),
            headers = Http4sEnvelope.toHeaders(request.headers),
            body = (Http4sEnvelope.toMediaType(request.headers), bytes)
          )
          decoder.decodeDetailed(endpoint.request, wire)
        .flatMap:
          case cats.data.Validated.Invalid(refused) => F.pure(Left(refused.failure))
          case cats.data.Validated.Valid(value)     =>
            evaluate(handler(value)).flatten
              .flatMap(value =>
                evaluate(encoder.encode(endpoint.responses, value)).handleErrorWith(cause =>
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
          val render = evaluate(encoder.encode(errors.responses, refused))
            .flatMap(Http4s.respond[F])
            .handleErrorWith: cause =>
              notify(Http4sObservation.Event.ErrorResponseFailed(cause)).attempt *> F.raiseError(cause)
          notify(Http4sObservation.Event.Failed(refused)) *> render

    F.onCancel(respond, notify(Http4sObservation.Event.Cancelled))

object Route:
  def apply[F[_], S[-_, +_], A, B, E, D](
      api: Api[S, E],
      endpoint: Endpoint.Declaration[S, Nothing, A, B, Any, D],
      handler: A => F[B]
  ): Route[F, S, A, B] =
    new Route(endpoint.domain, handler, endpoint.compose(api.errors).errors, endpoint.overrides)

  def apply[F[_], S[-_, +_], A, B, E](
      endpoint: Endpoint.WithErrors[S, Nothing, A, B, Any, E],
      handler: A => F[B]
  ): Route[F, S, A, B] =
    new Route(endpoint.domain, handler, endpoint.compose(ErrorPolicy.default).errors, endpoint.overrides)

  def apply[F[_], S[-_, +_], A, B, E](
      endpoint: ComposedEndpoint[S, Nothing, A, B, Any, E],
      handler: A => F[B]
  ): Route[F, S, A, B] = new Route(endpoint.domain, handler, endpoint.errors)

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
