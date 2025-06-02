package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.codec.PrimitivePrinter
import io.taig.otter.http.Query

object QuerySchemaPrimitivePrinter extends Encoder[Query.Schema.Primitive, String]:
  val printer = PrimitivePrinter(printer = this)(quotes = false)

  override def encode[A](schema: Query.Schema.Primitive[A], a: A): String =
    printer.encode(schema = schema.self, a)
