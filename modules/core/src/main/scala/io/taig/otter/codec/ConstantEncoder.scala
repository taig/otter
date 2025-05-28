package io.taig.otter.codec

import io.taig.otter.Constant

import scala.annotation.tailrec

final class ConstantEncoder[S[_], T](encoder: Encoder[S, T]) extends Encoder[Constant[S, *], T]:
  override def encode[A](schema: Constant[S, A], a: A): T = encode(schema = schema.value, a)

  @tailrec
  def encode[A](schema: Constant.Value[S, A], a: A): T = schema match
    case Constant.Value.Modify(self, _, g) => encode(schema = self, g(a))
    case Constant.Value.Root(schema, _)    => ReferenceConstantRenderer(encoder).render(reference = schema)
