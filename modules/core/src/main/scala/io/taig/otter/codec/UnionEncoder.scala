package io.taig.otter.codec

import io.taig.otter.Union

final class UnionEncoder[S[_], T](encoder: Encoder[S, T]) extends Encoder[Union[S, *], T]:
  def encode[A](schema: Union[S, A], a: A): T = schema match
    case Union.Root(schema, _)        => encoder.encode(schema = schema.value, a)
    case Union.OrElse(left, right, _) => a.fold(encode(left, _), encode(right, _))
    case Union.Modify(self, _, g)     => encode(schema = self, g(a))
