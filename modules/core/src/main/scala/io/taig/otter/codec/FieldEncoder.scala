package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Field

final class FieldEncoder[F[_], A](encoder: Encoder[F, A]) extends Encoder[Field[F, *], Chain[(String, A)]]:
  override def encode[B](field: Field[F, B], b: B): Chain[(String, A)] = field match
    case Field.Modify(self, _, g) => encode(self, g(b))
    case Field.Root(name, schema) => Chain.one(name -> encoder.encode(schema.value, b))