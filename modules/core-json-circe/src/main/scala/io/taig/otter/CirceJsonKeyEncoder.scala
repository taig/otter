package io.taig.otter

import scala.annotation.tailrec

object CirceJsonKeyEncoder:
  def apply[A](codec: Json.Key[A], a: A): String = apply(codec = codec.self, a)

  @tailrec
  def apply[A](codec: Primitive.String[A], a: A): String = codec match
    case _: Primitive.String.Text                          => a
    case Primitive.String.Modify(self, _, g)               => apply(codec = self, g(a))
    case Primitive.String.Parser(_, _, encode, _, _, _, _) => encode(a)
