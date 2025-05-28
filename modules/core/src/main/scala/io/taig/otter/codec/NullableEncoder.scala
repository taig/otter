package io.taig.otter.codec

import io.taig.otter.Nullable

final class NullableEncoder[S[_], T](encoder: Encoder[S, T], empty: T) extends Encoder[Nullable[S, *], T]:
  override def encode[A](schema: Nullable[S, A], a: A): T = encode(schema = schema.value, a)

  def encode[A](schema: Nullable.Value[S, A], a: A): T = schema match
    case Nullable.Value.Modify(self, _, g)    => encode(schema = self, g(a))
    case Nullable.Value.Default(reference, _) => encoder.encode(schema = reference.value, a)
    case Nullable.Value.Root(reference)       => a.fold(empty)(encoder.encode(schema = reference.value, _))
    case Nullable.Value.Void                  => empty
