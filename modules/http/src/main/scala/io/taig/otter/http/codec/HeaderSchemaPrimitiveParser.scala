package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.PrimitiveDecoder
import io.taig.otter.codec.PrimitiveParser
import io.taig.otter.http.Header
import io.taig.otter.http.Header.Schema.Primitive

object HeaderSchemaPrimitiveParser extends Decoder[Header.Schema.Primitive, String]:
  val parser = PrimitiveDecoder(decoder = PrimitiveParser(parser = this)(quotes = false))

  override def decode[A](schema: Primitive[A], value: String): Validated[Violations, A] =
    parser.decode(schema = schema.self, value)
