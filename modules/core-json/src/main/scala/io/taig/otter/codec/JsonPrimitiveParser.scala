package io.taig.otter.codec

import io.taig.otter.Json
import cats.data.Validated
import io.taig.otter.Violations

object JsonPrimitiveParser extends Decoder[Json.Primitive, String]:
  val parser = PrimitiveDecoder(decoder = PrimitiveParser(parser = this)(quotes = true))

  override def decode[A](schema: Json.Primitive[A], value: String): Validated[Violations, A] = 
    parser.decode(schema = schema.self, value)