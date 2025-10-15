package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Field
import cats.data.Chain

final class FieldEncoder[-S[_], T](encoder: Encoder[S, T]) extends Encoder[Field[S, *], Chain[(String, T)]]:
  override def encode[A](schema: Field[S, A], a: A): Chain[(String, T)] = schema match
    case Field.Default(self, _)   => encode(schema = self, a)
    case Field.Modify(self, _, g) => encode(schema = self, g(a))
    case Field.Optional(self)     => a.fold(Chain.empty)(Chain.one).flatMap(encode(schema = self, _))
    case Field.Root(name, schema) => Chain.one(name, encoder.encode(schema = schema.value, a))

object FieldEncoder:
  def apply[S[_], T](encoder: Encoder[S, T]): Encoder[Field[S, *], Chain[(String, T)]] = new FieldEncoder(encoder)
