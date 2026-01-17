package io.taig.otter.codec

import io.taig.otter.Union

final class UnionEncoder[F[_], T](encoder: Encoder[F, T]) extends Encoder[Union.Write[F, *], T]:
  override def encode[A](schema: Union.Write[F, A], a: A): T = schema match
    case Union.Coproduct(left, right)       => a.fold(encode(schema = left, _), encode(schema = right, _))
    case Union.Modify(self, _, f)           => encode(schema = self, f(a))
    case Union.Root(schema)                 => encoder.encode(schema.value, a)
    case Union.Write.Coproduct(left, right) => a.fold(encode(schema = left, _), encode(schema = right, _))
    case Union.Write.Modify(self, f)        => encode(schema = self, f(a))
