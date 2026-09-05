package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.http.Body
import io.taig.otter.http.Http4sIssue
import io.taig.otter.http.MediaType
import scodec.bits.ByteVector

/** Writes a body as the bytes it is, and the media type they go out under.
  *
  * The media type comes back rather than being read off the schema by the caller, because a
  * [[io.taig.otter.http.Bodies]] is a union and the caller does not know which alternative the value took --
  * [[io.taig.otter.codec.UnionEncoder]] does, and this is how it says so.
  *
  * `Either` because an [[Encoder]] has no failure channel and this one has something to fail at: an alphabet no
  * [[Http4sPayload]] recognises cannot be written, and the honest answer is to say which body could not be written
  * rather than to invent empty bytes for it. Both cases are shortfalls of the interpreter and neither depends on the
  * value, so a caller can find them before serving a single request.
  */
final class Http4sBodyEncoder(payload: Http4sPayload)
    extends Encoder[Body.Node, Either[Http4sIssue, (MediaType, ByteVector)]]:
  override def encode[W](body: Body.Node[W, Any], w: W): Either[Http4sIssue, (MediaType, ByteVector)] =
    encode(body.self.self, w)

  private def encode[W](
      body: Body.Value[Body.Payload, W, Any],
      w: W
  ): Either[Http4sIssue, (MediaType, ByteVector)] = body match
    case Body.Value.Modify(self, _, g)          => encode(self, g(w))
    case Body.Value.Whole(mediaType, reference) =>
      payload.encode(reference.value, w).toRight(Http4sIssue.Uninterpreted(mediaType)).map((mediaType, _))
    case Body.Value.Binary(mediaType)         => Right((mediaType, w))
    case Body.Value.Streamed(mediaType, _, _) => Left(Http4sIssue.Streamed(mediaType))
