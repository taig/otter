package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.NullableEncoder
import io.taig.otter.http.Parameter

object ParameterSchemaObjectAtomEncoder extends Encoder[Parameter.Schema.Object.Atom, Option[String]]:
  val nullable = NullableEncoder(encoder = this, empty = none)

  override def encode[A](schema: Parameter.Schema.Object.Atom[A], a: A): Option[String] = schema match
    case Parameter.Schema.Object.Atom.Nullable(self) => nullable.encode(schema = self.self, a)
    case schema: Parameter.Schema.Atom[A]            => ParameterSchemaAtomPrinter.encode(schema, a).some
