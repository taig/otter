package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Http4sWire
import io.taig.otter.http.Results

/** Interprets only body requirements covered by the supplied payloads. */
final class Http4sResultDecoder[P[-w, +r]](payload: Http4sPayload[P])
    extends Decoder[[w, r] =>> Results.Schema[Http4sPayload.Supported[P], w, r], Http4sWire.Response]:
  private val underlying = Http4sResultDecoderUnchecked(payload)

  override def decode[R](
      schema: Results.Schema[Http4sPayload.Supported[P], Nothing, R],
      value: Http4sWire.Response
  ): Validated[Violations, R] =
    underlying.decode(schema, value)
