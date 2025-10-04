package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Field

final class FieldEncoder[-S[_], T](encoder: Encoder[S, T]) extends Encoder[Field[S, *], Option[(String, T)]]:
  override def encode[A](schema: Field[S, A], a: A): Option[(String, T)] = schema match
    case Field.Default(self, _)   => encode(schema = self, a)
    case Field.Modify(self, _, g) => encode(schema = self, g(a))
    case Field.Optional(self)     => a.flatMap(encode(schema = self, _))
    case Field.Root(name, schema) => (name, encoder.encode(schema = schema.value, a)).some

object FieldEncoder:
  def apply[S[_], T](encoder: Encoder[S, T]) = new FieldEncoder(encoder)
