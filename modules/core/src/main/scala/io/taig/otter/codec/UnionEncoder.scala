package io.taig.otter.codec

import io.taig.otter.Union
import io.taig.otter.Union.Modify
import io.taig.otter.Union.OrElse
import io.taig.otter.Union.Root

final class UnionEncoder[-S[_], T](encoder: Encoder[S, T]) extends Encoder[Union[S, *], T]:
  override def encode[A](schema: Union[S, A], a: A): T = schema match
    case Union.Modify(self, _, g)  => encode(schema = self, g(a))
    case Union.OrElse(left, right) => a.fold(encode(schema = left, _), encode(schema = right, _))
    case Union.Root(schema)        => encoder.encode(schema = schema.value, a)

object UnionEncoder:
  def apply[S[_], A](encoder: Encoder[S, A]): Encoder[Union[S, *], A] = new UnionEncoder(encoder)
