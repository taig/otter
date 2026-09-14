package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Http4sWire
import io.taig.otter.http.Request

/** Interprets only body requirements covered by the supplied payloads. */
final class Http4sRequestDecoder[P[-w, +r]](payload: Http4sPayload[P])
    extends Decoder[[w, r] =>> Request.Schema[Http4sPayload.Supported[P], w, r], Http4sWire.Request]:
  private val underlying = Http4sRequestDecoderUnchecked(payload)

  override def decode[R](
      schema: Request.Schema[Http4sPayload.Supported[P], Nothing, R],
      value: Http4sWire.Request
  ): Validated[Violations, R] =
    underlying.decode(schema, value)

  /** Retains the error category as well as its structured violations. */
  def decodeDetailed[R](
      schema: Request.Schema[Http4sPayload.Supported[P], Nothing, R],
      value: Http4sWire.Request
  ): Validated[io.taig.otter.http.DecodingFailure, R] = underlying.decodeDetailed(schema, value)
