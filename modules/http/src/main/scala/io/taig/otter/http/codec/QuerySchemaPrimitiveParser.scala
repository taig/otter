package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.PrimitiveDecoder
import io.taig.otter.codec.PrimitiveParser
import io.taig.otter.http.Query

object QuerySchemaPrimitiveParser extends Decoder[Query.Schema.Primitive, String]:
  val parser = PrimitiveDecoder(decoder = PrimitiveParser(parser = this)(quotes = false))

  override def decode[A](schema: Query.Schema.Primitive[A], value: String): Validated[Violations, A] =
    parser.decode(schema = schema.self, value)
