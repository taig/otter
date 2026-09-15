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
  * `Either` because an [[Encoder]] has no failure channel and this one has something to fail at: a payload this
  * interpreter reads and writes can still have nothing to write, as a CSV document with no columns does. That an
  * alphabet might not be covered at all is no longer among the answers -- the requirement parameter settles it before a
  * route is built -- so what remains here depends on the value and on nothing else.
  */
final class Http4sBodyEncoder[P[-_, +_]](payload: Http4sPayload[P])
    extends Encoder[Body.Schema[Http4sPayload.Supported[P], *, *], Either[Http4sIssue, (MediaType, ByteVector)]]:
  override def encode[W](
      schema: Body.Schema[Http4sPayload.Supported[P], W, Any],
      value: W
  ): Either[Http4sIssue, (MediaType, ByteVector)] = encode(schema.self.self, value)

  private def encode[W](
      body: Body.Value[Http4sPayload.Supported[P], W, Any],
      w: W
  ): Either[Http4sIssue, (MediaType, ByteVector)] = body match
    case Body.Value.Modify(self, _, g)          => encode(self, g(w))
    case Body.Value.Whole(mediaType, reference) =>
      payload
        .encode(reference.value, w)
        .left
        .map(Http4sIssue.Encoding(mediaType, _))
        .map((mediaType, _))
    case Body.Value.Binary(mediaType) => Right((mediaType, w))
