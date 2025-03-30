package io.taig.otter

import scala.annotation.tailrec

object StringPrimitivePrinter:
  @tailrec
  def apply[A](codec: Primitive[?, A], value: A): String = codec match
    case _: Primitive.String                        => value
    case _: Primitive.BigDecimal                    => value.toPlainString
    case _: Primitive.BigInteger                    => value.toString
    case _: Primitive.Float                         => String.valueOf(value)
    case _: Primitive.Double                        => String.valueOf(value)
    case _: Primitive.Int                           => String.valueOf(value)
    case _: Primitive.Long                          => String.valueOf(value)
    case _: Primitive.Boolean                       => String.valueOf(value)
    case Primitive.Modify(self, _, g)               => StringPrimitivePrinter(self, g(value))
    case Primitive.Parser(_, _, encode, _, _, _, _) => encode(value)
