package io.taig.otter.codec

import io.taig.otter.Json

object JsonPrimitivePrinter extends Encoder[Json.Primitive, String]:
  val printer = PrimitivePrinter(printer = this)(quotes = true)

  override def encode[A](schema: Json.Primitive[A], a: A): String =  printer.encode(schema = schema.self, a)