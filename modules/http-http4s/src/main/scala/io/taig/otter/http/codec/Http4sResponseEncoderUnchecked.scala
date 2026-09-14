package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.UnionEncoder
import io.taig.otter.http.Body
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
  */
final private[http] class Http4sResponseEncoderUnchecked(payload: Http4sPayload[?])
    extends Encoder[Responses.Node, Either[Http4sIssue, Http4sWire.Response]]:
  private val responses = UnionEncoder(Http4sResponseEncoderUnchecked.One(payload))

  override def encode[W](responses: Responses.Node[W, Any], w: W): Either[Http4sIssue, Http4sWire.Response] =
    this.responses.encode(responses.self.self, w)

private[http] object Http4sResponseEncoderUnchecked:
  /** One branch of the union, which is one status and what goes out under it. */
  final private[http] class One(payload: Http4sPayload[?])
      extends Encoder[Response.Node, Either[Http4sIssue, Http4sWire.Response]]:
    private val bodies = UnionEncoder(Http4sBodyEncoderUnchecked(payload))

    override def encode[W](response: Response.Node[W, Any], w: W): Either[Http4sIssue, Http4sWire.Response] =
      encode(response.self.self, w)

    private def encode[W](
        response: Response.Value[Body.Payload, W, Any],
        w: W
    ): Either[Http4sIssue, Http4sWire.Response] = response match
      case Response.Value.Root(status)           => Right(Http4sWire.Response(status, Chain.empty, None))
      case Response.Value.Headers(self, headers) =>
        encode(self, w._1).map(wire => wire.copy(headers = wire.headers ++ HeadersEncoder.encode(headers.value, w._2)))
      case Response.Value.Entity(self, values) =>
        for
          wire <- encode(self, w._1)
          body <- bodies.encode(values.value.self.self, w._2)
        yield wire.copy(body = Some(body))
      // A response that promised a stream cannot be answered with one here, and answering it with an empty body would
      // be a lie the caller could not detect. It is reported instead.
      case Response.Value.Streamed(_, value) => Left(Http4sIssue.Streamed(value.value.mediaType))
      case Response.Value.Modify(self, _, g) => encode(self, g(w))
