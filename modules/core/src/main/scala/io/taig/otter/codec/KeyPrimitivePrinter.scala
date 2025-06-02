package io.taig.otter.codec

import io.taig.otter.Key

final class KeyPrimitivePrinter(quotes: Boolean) extends Encoder[Key.Primitive, String]:
  val printer = PrimitivePrinter(printer = this)(quotes)

  override def encode[A](schema: Key.Primitive[A], a: A): String =
    printer.encode(schema = schema.self, a)

object KeyPrimitivePrinter:
  val Quoted: Encoder[Key.Primitive, String] = KeyPrimitivePrinter(quotes = true)
  val Unquoted: Encoder[Key.Primitive, String] = KeyPrimitivePrinter(quotes = false)
