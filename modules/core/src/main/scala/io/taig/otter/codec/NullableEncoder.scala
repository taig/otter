package io.taig.otter.codec

import io.taig.otter.Nullable
import io.taig.otter.Nullable.Default
import io.taig.otter.Nullable.Modify
import io.taig.otter.Nullable.Root

final class NullableEncoder[S[_], T](encoder: Encoder[S, T], empty: T) extends Encoder[Nullable[S, *], T]:
  override def encode[A](schema: Nullable[S, A], a: A): T = schema match
    case Nullable.Modify(self, _, g)    => encode(schema = self, g(a))
    case Nullable.Default(reference, _) => encoder.encode(schema = reference.value, a)
    case Nullable.Root(reference)       => a.fold(empty)(encoder.encode(schema = reference.value, _))
    case Nullable.Void                  => empty
