package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.UnionEncoder
import io.taig.otter.http.Body
import io.taig.otter.http.Http4sIssue
import io.taig.otter.http.Http4sWire
import io.taig.otter.http.Request
import scodec.bits.ByteVector

/** Writes a request from what it holds, which is the same walk [[Http4sRequestDecoder]] makes in reverse.
  *
  * The two are written apart rather than as one round trip because they answer to different sides of the schema, and a
  * field that is optional or defaulted genuinely differs between them. What keeps them honest is that the same endpoint
  * value drives both, so a disagreement is a test failure rather than a silent divergence.
  */
final class Http4sRequestEncoder(payload: Http4sPayload)
    extends Encoder[Request.Node, Either[Http4sIssue, Http4sWire.Request]]:
  private val bodies = UnionEncoder(Http4sBodyEncoder(payload))

  override def encode[W](request: Request.Node[W, Any], w: W): Either[Http4sIssue, Http4sWire.Request] =
    encode(request.self.self, w)

  private def encode[W](
      request: Request.Value[Body.Payload, W, Any],
      w: W
  ): Either[Http4sIssue, Http4sWire.Request] = request match
    case Request.Value.Root(_, path) =>
      Right(Http4sWire.Request(PathEncoder.encode(path.value, w), Chain.empty, Chain.empty, (None, ByteVector.empty)))
    case Request.Value.Queries(self, queries) =>
      encode(self, w._1).map(wire => wire.copy(queries = wire.queries ++ QueriesEncoder.encode(queries.value, w._2)))
    case Request.Value.Headers(self, headers) =>
      encode(self, w._1).map(wire => wire.copy(headers = wire.headers ++ HeadersEncoder.encode(headers.value, w._2)))
    case Request.Value.Payload(self, values) =>
      for
        wire <- encode(self, w._1)
        body <- bodies.encode(values.value.self.self, w._2)
      yield wire.copy(body = (Some(body._1), body._2))
    case Request.Value.Streaming(self, _) => encode(self, w)
    case Request.Value.Modify(self, _, g) => encode(self, g(w))
