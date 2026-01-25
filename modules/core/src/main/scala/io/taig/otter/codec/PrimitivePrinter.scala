package io.taig.otter.codec

import io.taig.otter.Primitive

import scala.annotation.tailrec

object PrimitivePrinter extends Printer[Primitive.Write]:
  @tailrec
  override def encode[A](schema: Primitive.Write[A], a: A): String = schema match
    case Primitive.Boolean.Modify(self, _, f)    => encode(schema = self, f(a))
    case Primitive.Boolean.Root                  => String.valueOf(a)
    case Primitive.Boolean.Write.Modify(self, f) => encode(schema = self, f(a))
    case Primitive.Modify(self, _, f)            => encode(schema = self, f(a))
    case Primitive.Number.BigDecimal(_)          => a.toPlainString
    case Primitive.Number.BigInteger(_)          => String.valueOf(a)
    case Primitive.Number.Double(_)              => String.valueOf(a)
    case Primitive.Number.Float(_)               => String.valueOf(a)
    case Primitive.Number.Int(_)                 => String.valueOf(a)
    case Primitive.Number.Long(_)                => String.valueOf(a)
    case Primitive.Number.Modify(self, _, f)     => encode(schema = self, f(a))
    case Primitive.Number.Write.Modify(self, f)  => encode(schema = self, f(a))
    case Primitive.Text.Codec(_, _, print)       => print(a)
    case Primitive.Text.Modify(self, _, f)       => encode(schema = self, f(a))
    case Primitive.Text.Root(_)                  => a
    case Primitive.Text.Write.Modify(self, f)    => encode(schema = self, f(a))
    case Primitive.Text.Write.Printer(_, print)  => print(a)
