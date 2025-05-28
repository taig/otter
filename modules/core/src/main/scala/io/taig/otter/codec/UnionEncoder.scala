package io.taig.otter.codec

import io.taig.otter.Union

final class UnionEncoder[S[_], T](encoder: Encoder[S, T]) extends Encoder[Union[S, *], T]:
  override def encode[A](schema: Union[S, A], a: A): T = encode(schema = schema.value, a)

  def encode[A](schema: Union.Value[S, A], a: A): T = schema match
    case Union.Value.Root(schema)        => encoder.encode(schema = schema.value, a)
    case Union.Value.OrElse(left, right) => a.fold(encode(left, _), encode(right, _))
    case Union.Value.Modify(self, _, g)  => encode(schema = self, g(a))
