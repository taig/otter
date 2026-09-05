package io.taig.otter.http

import cats.effect.Concurrent
import cats.syntax.all.*
import io.taig.otter.http.codec.Http4sRequestDecoder
import io.taig.otter.http.codec.Http4sResultEncoder
import io.taig.otter.http.codec.PathTemplate
import org.http4s.Request as Http4sRequest
import org.http4s.Response as Http4sResponse
import scodec.bits.ByteVector

/** An endpoint, and what answers it.
  *
  * `A => F[B]` and nothing wider is the whole of what a handler is, because the endpoint has already said what a
  * request holds and what an answer may be. There is no request object to reach into and no response builder to get
  * wrong: a status code is chosen by which branch of the result union the value took, and a handler that returns the
  * wrong shape does not compile.
  */
final case class Route[F[_], A, B](endpoint: Endpoint.Server[Body.Payload, A, B], handler: A => F[B]):
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
  def run(
      decoder: Http4sRequestDecoder,
      encoder: Http4sResultEncoder,
      request: Http4sRequest[F],
      segments: Vector[String]
  )(using
      F: Concurrent[F]
  ): F[Http4sResponse[F]] =
    Route
      .bytes(endpoint, request)
      .flatMap: bytes =>
        val wire = Http4sWire.Request(
          path = segments,
          queries = Http4sEnvelope.toQueries(request.uri.query),
          headers = Http4sEnvelope.toHeaders(request.headers),
          body = (Http4sEnvelope.toMediaType(request.headers), bytes)
        )

        decoder.decode(endpoint.request, wire) match
          case cats.data.Validated.Valid(value)        => handler(value).map(encoder.encode(endpoint.responses, _))
          case cats.data.Validated.Invalid(violations) => F.pure(Http4s.malformed(violations))
      .flatMap(Http4s.respond[F])

object Route:
  /** The request's bytes, read only if the endpoint describes something to read them as.
    *
    * An endpoint with no body never touches the entity at all, which is what keeps a `GET` from paying for a stream it
    * was never going to look at.
    */
  private def bytes[F[_]: Concurrent](
      endpoint: Endpoint.Server[Body.Payload, ?, ?],
      request: Http4sRequest[F]
  ): F[ByteVector] =
    if endpoint.request.bodies.isEmpty then ByteVector.empty.pure else Http4sEnvelope.toBytes(request.entity)
