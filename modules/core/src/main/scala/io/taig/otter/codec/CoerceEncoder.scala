package io.taig.otter.codec

import io.taig.otter.Coerce

final class CoerceEncoder[F[_], T](encoder: Encoder[F, T]) extends Encoder[Coerce.Write[F, *], T]:
  override def encode[A](schema: Coerce.Write[F, A], a: A): T = schema match
    case Coerce.Root(schema)          => encoder.encode(schema.value, a)
    case Coerce.Modify(self, _, f)    => encode(schema = self, f(a))
    case Coerce.Write.Modify(self, f) => encode(schema = self, f(a))
