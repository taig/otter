package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Field

final class FieldEncoder[F[_], A](encoder: Encoder[F, A]) extends Encoder[Field.Write[F, *], Chain[(String, A)]]:
  override def encode[B](field: Field.Write[F, B], b: B): Chain[(String, A)] = field match
    case Field.Default(self, _)      => encode(field = self, b)
    case Field.Modify(self, _, f)    => encode(self, f(b))
    case Field.Optional(self)        => b.fold(Chain.empty)(encode(self, _))
    case Field.Root(name, schema)    => Chain.one(name -> encoder.encode(schema.value, b))
    case Field.Write.Modify(self, f) => encode(self, f(b))
    case Field.Write.Optional(self)  => b.fold(Chain.empty)(encode(self, _))
