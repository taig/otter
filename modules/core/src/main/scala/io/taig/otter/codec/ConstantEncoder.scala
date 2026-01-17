package io.taig.otter.codec

import io.taig.otter.Constant

import scala.annotation.tailrec

final class ConstantEncoder[F[_], T](encoder: Encoder[F, T]) extends Encoder[Constant.Write[F, *], T]:
  @tailrec
  override def encode[A](schema: Constant.Write[F, A], a: A): T = schema match
    case Constant.Modify(self, _, f)        => encode(schema = self, f(a))
    case Constant.Root(schema, value, _)    => encoder.encode(schema.value, value.value)
    case Constant.Write.Modify(self, f)     => encode(schema = self, f(a))
    case Constant.Write.Root(schema, value) => encoder.encode(schema.value, value.value)
