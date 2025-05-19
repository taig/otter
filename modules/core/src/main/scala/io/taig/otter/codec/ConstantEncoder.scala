package io.taig.otter.codec

import io.taig.otter.Constant
import io.taig.otter.Constant.Modify
import io.taig.otter.Constant.Root

import scala.annotation.tailrec

final class ConstantEncoder[S[_], T](encoder: Encoder[S, T]) extends Encoder[Constant[S, *], T]:
  @tailrec
  override def encode[A](schema: Constant[S, A], a: A): T = schema match
    case Constant.Modify(self, _, g) => encode(schema = self, g(a))
    case Constant.Root(schema, _, _) => ReferenceConstantEncoder(encoder)(reference = schema)
