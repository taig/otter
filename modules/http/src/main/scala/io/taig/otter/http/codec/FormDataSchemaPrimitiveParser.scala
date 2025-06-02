package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.PrimitiveParser
import io.taig.otter.http.FormData

object FormDataSchemaPrimitiveParser extends Decoder[FormData.Schema.Primitive, String]:
  override def decode[A](schema: FormData.Schema.Primitive[A], value: String): Validated[Violations, A] =
    PrimitiveParser(parser = this)(quotes = false).decode(schema = schema.self, value)
