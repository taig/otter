package io.taig.otter

object PrimitivePrinter extends Printer[Primitive]:
  override def apply[A](codec: Primitive[A], a: A): String = codec match
    case _: Primitive.Boolean.Root                         => String.valueOf(a)
    case _: Primitive.Number.BigDecimal                    => a.toPlainString
    case _: Primitive.Number.BigInteger                    => a.toString
    case _: Primitive.Number.Double                        => String.valueOf(a)
    case _: Primitive.Number.Float                         => String.valueOf(a)
    case _: Primitive.Number.Int                           => String.valueOf(a)
    case _: Primitive.Number.Long                          => String.valueOf(a)
    case _: Primitive.String.Text                          => s"\"${a.replace("\"", "\\\"")}\""
    case Primitive.Boolean.Modify(self, _, g)              => apply(codec = self, g(a))
    case Primitive.Number.Modify(self, _, g)               => apply(codec = self, g(a))
    case Primitive.String.Modify(self, _, g)               => apply(codec = self, g(a))
    case Primitive.String.Parser(_, _, encode, _, _, _, _) => s"\"${encode(a)}\""
