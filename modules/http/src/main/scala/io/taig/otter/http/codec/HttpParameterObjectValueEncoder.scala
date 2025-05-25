package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.NullableEncoder
import io.taig.otter.http.Parameter

object HttpParameterObjectValueEncoder extends Encoder[Parameter.Value.Object.Atom, Option[String]]:
  val nullable = NullableEncoder(encoder = this, empty = none)

  override def encode[A](schema: Parameter.Value.Object.Atom[A], a: A): Option[String] = schema match
    case Parameter.Value.Object.Atom.Nullable(self) => nullable.encode(schema = self.self, a)
    case schema: Parameter.Value.Atom[A]            => ParameterValueAtomPrinter.encode(schema, a).some
