package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Key
import io.taig.otter.Violations

final class KeyPrimitiveParser(quotes: Boolean) extends Decoder[Key.Primitive, String]:
  val parser = PrimitiveDecoder(decoder = PrimitiveParser(parser = this)(quotes))

  override def decode[A](schema: Key.Primitive[A], value: String): Validated[Violations, A] =
    parser.decode(schema = schema.self, value)

object KeyPrimitiveParser:
  val Quoted: Decoder[Key.Primitive, String] = KeyPrimitiveParser(quotes = true)
  val Unquoted: Decoder[Key.Primitive, String] = KeyPrimitiveParser(quotes = false)
