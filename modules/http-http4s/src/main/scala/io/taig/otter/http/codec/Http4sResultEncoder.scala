package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.http.Http4sIssue
import io.taig.otter.http.Http4sWire
import io.taig.otter.http.Results

/** Interprets only body requirements covered by the supplied payloads. */
final class Http4sResultEncoder[P[-w, +r]](payload: Http4sPayload[P])
    extends Encoder[[w,
    r] =>> Results.Schema[Http4sPayload.Supported[P], w, r], Either[Http4sIssue, Http4sWire.Response]]:
  private val underlying = Http4sResultEncoderUnchecked(payload)

  override def encode[W](
      schema: Results.Schema[Http4sPayload.Supported[P], W, Any],
      value: W
  ): Either[Http4sIssue, Http4sWire.Response] =
    underlying.encode(schema, value)
