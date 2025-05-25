package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.http.Header
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.NullableEncoder

object HeaderValueObjectValueEncoder extends Encoder[Header.Value.Object.Atom, Option[String]]:
  val nullable = NullableEncoder(encoder = this, empty = none)

  override def encode[A](schema: Header.Value.Object.Atom[A], a: A): Option[String] = schema match
    case Header.Value.Object.Atom.Nullable(self) => nullable.encode(schema = self.self, a)
    case schema: Header.Value.Atom[A]            => HeaderValueAtomPrinter.encode(schema, a).some
