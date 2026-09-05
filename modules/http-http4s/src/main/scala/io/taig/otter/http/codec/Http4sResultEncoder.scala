package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.UnionEncoder
import io.taig.otter.http.Body
import io.taig.otter.http.Http4sIssue
import io.taig.otter.http.Http4sWire
import io.taig.otter.http.Result
import io.taig.otter.http.Results

/** Writes the answer an endpoint gave, and the status it gave it under.
  *
  * The status is not chosen here and not passed in: it is on the [[Result.Value.Root]] of whichever branch of the union
  * the value turned out to be, and [[UnionEncoder]] is what folds an `Either` nest down to that one branch. That is the
  * whole reason responses are described as a union rather than as a status beside a body -- a handler returning
  * `Left(report)` has already said `200`, and no second decision is needed.
  */
final class Http4sResultEncoder(payload: Http4sPayload)
    extends Encoder[Results.Node, Either[Http4sIssue, Http4sWire.Response]]:
  private val results = UnionEncoder(Http4sResultEncoder.One(payload))

  override def encode[W](results: Results.Node[W, Any], w: W): Either[Http4sIssue, Http4sWire.Response] =
    this.results.encode(results.self.self, w)

object Http4sResultEncoder:
  /** One branch of the union, which is one status code and what goes out under it. */
  final class One(payload: Http4sPayload) extends Encoder[Result.Node, Either[Http4sIssue, Http4sWire.Response]]:
    private val bodies = UnionEncoder(Http4sBodyEncoder(payload))

    override def encode[W](result: Result.Node[W, Any], w: W): Either[Http4sIssue, Http4sWire.Response] =
      encode(result.self.self, w)

    private def encode[W](
        result: Result.Value[Body.Payload, W, Any],
        w: W
    ): Either[Http4sIssue, Http4sWire.Response] = result match
      case Result.Value.Root(code)             => Right(Http4sWire.Response(code, Chain.empty, None))
      case Result.Value.Headers(self, headers) =>
        encode(self, w._1).map(wire => wire.copy(headers = wire.headers ++ HeadersEncoder.encode(headers.value, w._2)))
      case Result.Value.Payload(self, values) =>
        for
          wire <- encode(self, w._1)
          body <- bodies.encode(values.value.self.self, w._2)
        yield wire.copy(body = Some(body))
      // A result that promised a stream cannot be answered with one here, and answering it with an empty body would
      // be a lie the caller could not detect. It is reported instead.
      case Result.Value.Streaming(_, value) => Left(Http4sIssue.Streamed(value.value.mediaType))
      case Result.Value.Modify(self, _, g)  => encode(self, g(w))
