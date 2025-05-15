package io.taig.otter.codec

import io.taig.otter.Field
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Field.Modify
import io.taig.otter.Field.Root
import io.taig.otter.Field.Optional

final class FieldEncoder[S[_], T[_], U, V](key: Encoder[S, U], value: Encoder[T, V])
    extends Encoder[Field[S, T, *], Option[(U, V)]]:
  override def apply[A](schema: Field[S, T, A], a: A): Option[(U, V)] = schema match
    case Field.Modify(self, f, g) => apply(schema = self, g(a))
    case Field.Nullish(self)      => apply(schema, a)
    case Field.Optional(self)     => a.flatMap(apply(schema = self, _))
    case Field.Root(key, value, _) =>
      (
        ReferenceConstantEncoder(encoder = this.key)(key),
        ReferenceEncoder(encoder = this.value)(value, a)
      ).some
