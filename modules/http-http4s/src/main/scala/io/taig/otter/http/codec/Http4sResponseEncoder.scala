package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.EitherT
import cats.effect.Concurrent
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.UnionEncoder
import io.taig.otter.http.Http4sIssue
import io.taig.otter.http.Http4sWire
import io.taig.otter.http.Response
import io.taig.otter.http.Responses

/** Writes the answer an endpoint gave, and the status it gave it under.
  *
  * The status is not chosen here and not passed in: it is on the [[Response.Value.Root]] of whichever branch of the
  * union the value turned out to be, and [[UnionEncoder]] is what folds an `Either` nest down to that one branch. That
  * is the whole reason responses are described as a union rather than as a status beside a body -- a handler returning
  * `Left(report)` has already said `200`, and no second decision is needed.
  *
  * A response that promised a stream is not a case here. Its requirement is [[io.taig.otter.http.Body.Requirement]]'s
  * streamed form, which no `Supported[P]` admits, so an endpoint describing one cannot be served by this backend and
  * cannot reach this walk.
  */
final class Http4sResponseEncoder[F[_]: Concurrent, P[-_, +_]](payload: Http4sPayload[P])
    extends Encoder[Responses.Schema[Http4sPayload.Supported[P], *, *], F[Either[Http4sIssue, Http4sWire.Response]]]:
  private val responses = UnionEncoder(Http4sResponseEncoder.One[F, P](payload))

  override def encode[W](
      schema: Responses.Schema[Http4sPayload.Supported[P], W, Any],
      value: W
  ): F[Either[Http4sIssue, Http4sWire.Response]] = responses.encode(schema.self.self, value)

private[http] object Http4sResponseEncoder:
  /** One branch of the union, which is one status and what goes out under it. */
  final private[http] class One[F[_]: Concurrent, P[-_, +_]](payload: Http4sPayload[P])
      extends Encoder[Response.Schema[Http4sPayload.Supported[P], *, *], F[Either[Http4sIssue, Http4sWire.Response]]]:
    private val bodies = UnionEncoder(Http4sBodyEncoder[F, P](payload))

    override def encode[W](
        response: Response.Schema[Http4sPayload.Supported[P], W, Any],
        value: W
    ): F[Either[Http4sIssue, Http4sWire.Response]] = encode(response.self.self, value).value

    private def encode[W](
        response: Response.Value[Http4sPayload.Supported[P], W, Any],
        w: W
    ): EitherT[F, Http4sIssue, Http4sWire.Response] = response match
      case Response.Value.Root(status)           => EitherT.rightT(Http4sWire.Response(status, Chain.empty, None))
      case Response.Value.Headers(self, headers) =>
        encode(self, w._1).map(wire => wire.copy(headers = wire.headers ++ HeadersEncoder.encode(headers.value, w._2)))
      case Response.Value.Entity(self, values) =>
        for
          wire <- encode(self, w._1)
          body <- EitherT(bodies.encode(values.value.self.self, w._2))
        yield wire.copy(body = Some(body))
      case Response.Value.Modify(self, _, g) => encode(self, g(w))
