package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Field

final class FieldEncoder[F[_], T](encoder: Encoder[F, T]) extends Encoder[Field.Write[F, *], Chain[(String, T)]]:
  override def encode[A](field: Field.Write[F, A], a: A): Chain[(String, T)] = field match
    case Field.Default(self, _)      => encode(field = self, a)
    case Field.Modify(self, _, f)    => encode(self, f(a))
    case Field.Optional(self)        => a.fold(Chain.empty)(encode(self, _))
    case Field.Root(name, schema)    => Chain.one(name -> encoder.encode(schema.value, a))
    case Field.Write.Modify(self, f) => encode(self, f(a))
    case Field.Write.Optional(self)  => a.fold(Chain.empty)(encode(self, _))
