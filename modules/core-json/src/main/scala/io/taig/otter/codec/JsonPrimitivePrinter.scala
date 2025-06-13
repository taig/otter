package io.taig.otter.codec

import io.taig.otter.Json

final class JsonPrimitivePrinter(quotes: Boolean) extends Encoder[Json.Primitive, String]:
  val printer = PrimitivePrinter(printer = this)(quotes)

  override def encode[A](schema: Json.Primitive[A], a: A): String = printer.encode(schema = schema.self, a)

object JsonPrimitivePrinter:
  val Quoted: Encoder[Json.Primitive, String] = JsonPrimitivePrinter(quotes = true)
  val Unquoted: Encoder[Json.Primitive, String] = JsonPrimitivePrinter(quotes = false)
