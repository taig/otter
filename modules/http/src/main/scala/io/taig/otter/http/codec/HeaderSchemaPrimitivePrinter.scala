package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.codec.PrimitivePrinter
import io.taig.otter.http.Header

object HeaderSchemaPrimitivePrinter extends Encoder[Header.Schema.Primitive, String]:
  val printer = PrimitivePrinter(printer = this)(quotes = false)

  override def encode[A](schema: Header.Schema.Primitive[A], a: A): String =
    printer.encode(schema = schema.self, a)
