package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Body
import io.taig.otter.http.MediaType
import scodec.bits.ByteVector

/** Interprets only body requirements covered by the supplied payloads. */
final class Http4sBodyDecoder[P[-_, +_]](payload: Http4sPayload[P])
    extends Decoder[Body.Schema[Http4sPayload.Supported[P], *, *], (Option[MediaType], ByteVector)]:
  private val underlying = Http4sBodyDecoderUnchecked(payload)

  override def decode[R](
      schema: Body.Schema[Http4sPayload.Supported[P], Nothing, R],
      value: (Option[MediaType], ByteVector)
  ): Validated[Violations, R] =
    underlying.decode(schema, value)

  /** Retains the error category as well as its structured violations. */
  def decodeDetailed[R](
      schema: Body.Schema[Http4sPayload.Supported[P], Nothing, R],
      value: (Option[MediaType], ByteVector)
  ): Validated[io.taig.otter.http.DecodingFailure, R] = underlying.decodeDetailed(schema, value)
