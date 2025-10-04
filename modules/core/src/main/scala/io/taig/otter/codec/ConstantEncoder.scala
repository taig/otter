package io.taig.otter.codec

import io.taig.otter.Constant

final class ConstantEncoder[-S[_], T](encoder: Encoder[S, T]) extends Encoder[Constant[S, *], T]:
  override def encode[A](schema: Constant[S, A], a: A): T = schema match
    case Constant.Modify(self, _, g)     => encode(schema = self, g(a))
    case Constant.Root(schema, value, _) => encoder.encode(schema = schema.value, value)

object ConstantEncoder:
  def apply[S[_], A](encoder: Encoder[S, A]): Encoder[Constant[S, *], A] = new ConstantEncoder(encoder)
