package io.taig.otter

object PrimitivePrinter:
  def apply[A](codec: Primitive[A], a: A): String = codec match
    case _: Primitive.BigDecimal                    => a.toPlainString
    case _: Primitive.BigInteger                    => a.toString
    case _: Primitive.Boolean                       => String.valueOf(a)
    case _: Primitive.Double                        => String.valueOf(a)
    case _: Primitive.Float                         => String.valueOf(a)
    case _: Primitive.Int                           => String.valueOf(a)
    case _: Primitive.Long                          => String.valueOf(a)
    case Primitive.Modify(self, _, g)               => apply(codec = self, g(a))
    case Primitive.Parser(_, _, encode, _, _, _, _) => s"\"${encode(a)}\""
    case _: Primitive.String                        => s"\"$a\""
