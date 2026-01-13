package io.taig.otter.codec

import io.taig.otter.Constant

import scala.annotation.tailrec

final class ConstantEncoder[F[_], A](encoder: Encoder[F, A]) extends Encoder[Constant.Write[F, *], A]:
  @tailrec
  override def encode[B](schema: Constant.Write[F, B], a: B): A = schema match
    case Constant.Modify(self, _, f)        => encode(schema = self, f(a))
    case Constant.Root(schema, value, _, _)    => encoder.encode(schema.value, value.value)
    case Constant.Write.Modify(self, f)     => encode(schema = self, f(a))
    case Constant.Write.Root(schema, value, _) => encoder.encode(schema.value, value.value)
