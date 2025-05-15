package io.taig.otter.codec

import io.taig.otter.Field
import cats.data.Chain
import io.taig.otter.Field.Modify
import io.taig.otter.Field.Root
import io.taig.otter.Field.Optional

final class FieldEncoder[S[_], T[_], U](key: Encoder[S, String], value: Encoder[T, U]):
  def apply[A](schema: Field[S, T, A], a: A): Chain[(String, U)] = schema match
    case Field.Modify(self, f, g) => apply(schema = self, g(a))
    case Field.Root(key, value, _) =>
      Chain.one(
        ReferenceConstantEncoder(encoder = this.key)(key) -> ReferenceEncoder(encoder = this.value)(value, a)
      )
    case Field.Optional(self) => a.fold(Chain.empty)(apply(schema = self, _))
