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
  * Streamed entities remain lazy while the response envelope is written. Element failures therefore terminate the
  * outgoing stream; they cannot replace a status already sent.
  */
final class Http4sResponseEncoder[F[_]: Concurrent, P[-_, +_], Q[-_, +_]](payload: Http4sInterpreter.Of[P, Q])
    extends Encoder[Responses.Schema[Http4sInterpreter.Supported[F, P, Q], *, *], F[
      Either[Http4sIssue, Http4sWire.Response[F]]
    ]]:
  private val responses = UnionEncoder(Http4sResponseEncoder.One[F, P, Q](payload))

  override def encode[W](
      schema: Responses.Schema[Http4sInterpreter.Supported[F, P, Q], W, Any],
      value: W
  ): F[Either[Http4sIssue, Http4sWire.Response[F]]] = responses.encode(schema.self.self, value)

private[http] object Http4sResponseEncoder:
  /** One branch of the union, which is one status and what goes out under it. */
  final private[http] class One[F[_]: Concurrent, P[-_, +_], Q[-_, +_]](payload: Http4sInterpreter.Of[P, Q])
      extends Encoder[Response.Schema[Http4sInterpreter.Supported[F, P, Q], *, *], F[
        Either[Http4sIssue, Http4sWire.Response[F]]
      ]]:
    private val bodies = UnionEncoder(Http4sBodyEncoder[F, P, Q](payload))

    override def encode[W](
        response: Response.Schema[Http4sInterpreter.Supported[F, P, Q], W, Any],
        value: W
    ): F[Either[Http4sIssue, Http4sWire.Response[F]]] = encode(response.self.self, value).value

    private def encode[W](
        response: Response.Value[Http4sInterpreter.Supported[F, P, Q], W, Any],
        w: W
    ): EitherT[F, Http4sIssue, Http4sWire.Response[F]] = response match
      case Response.Value.Root(status) =>
        EitherT.rightT(Http4sWire.Response[F](status, Chain.empty, (None, org.http4s.Entity.empty[F])))
      case Response.Value.Headers(self, headers) =>
        encode(self, w._1).map(wire => wire.copy(headers = wire.headers ++ HeadersEncoder.encode(headers.value, w._2)))
      case Response.Value.Entity(self, values) =>
        for
          wire <- encode(self, w._1)
          body <- EitherT(bodies.encode(values.value.self.self, w._2))
        yield wire.copy(body = (Some(body._1), body._2))
      case Response.Value.Modify(self, _, g) => encode(self, g(w))
