package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.http.Body
import io.taig.otter.http.Http4sIssue
import io.taig.otter.http.MediaType
import scodec.bits.ByteVector

/** Interprets only body requirements covered by the supplied payloads. */
final class Http4sBodyEncoder[P[-w, +r]](payload: Http4sPayload[P])
    extends Encoder[[w,
    r] =>> Body.Schema[Http4sPayload.Supported[P], w, r], Either[Http4sIssue, (MediaType, ByteVector)]]:
  private val underlying = Http4sBodyEncoderUnchecked(payload)

  override def encode[W](
      schema: Body.Schema[Http4sPayload.Supported[P], W, Any],
      value: W
  ): Either[Http4sIssue, (MediaType, ByteVector)] =
    underlying.encode(schema, value)
