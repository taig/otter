package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.codec.PrimitivePrinter
import io.taig.otter.http.FormData

object FormDataSchemaPrimitivePrinter extends Encoder[FormData.Schema.Primitive, String]:
  override def encode[A](schema: FormData.Schema.Primitive[A], a: A): String =
    PrimitivePrinter(printer = this)(quotes = false).encode(schema = schema.self, a)
