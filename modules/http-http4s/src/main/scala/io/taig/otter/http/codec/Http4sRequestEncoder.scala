package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.http.Http4sIssue
import io.taig.otter.http.Http4sWire
import io.taig.otter.http.Request

/** Interprets only body requirements covered by the supplied payloads. */
final class Http4sRequestEncoder[P[-w, +r]](payload: Http4sPayload[P])
    extends Encoder[[w,
    r] =>> Request.Schema[Http4sPayload.Supported[P], w, r], Either[Http4sIssue, Http4sWire.Request]]:
  private val underlying = Http4sRequestEncoderUnchecked(payload)

  override def encode[W](
      schema: Request.Schema[Http4sPayload.Supported[P], W, Any],
      value: W
  ): Either[Http4sIssue, Http4sWire.Request] =
    underlying.encode(schema, value)
