package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Http4sWire
import io.taig.otter.http.Responses

/** Interprets only body requirements covered by the supplied payloads. */
final class Http4sResponseDecoder[P[-_, +_]](payload: Http4sPayload[P])
    extends Decoder[Responses.Schema[Http4sPayload.Supported[P], *, *], Http4sWire.Response]:
  private val underlying = Http4sResponseDecoderUnchecked(payload)

  override def decode[R](
      schema: Responses.Schema[Http4sPayload.Supported[P], Nothing, R],
      value: Http4sWire.Response
  ): Validated[Violations, R] =
    underlying.decode(schema, value)
