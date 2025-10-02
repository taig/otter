package io.taig.otter.codec

import io.taig.otter.Primitive

import scala.annotation.tailrec

object PrimitivePrinter extends Printer[Primitive]:
  @tailrec
  override def print[A](schema: Primitive[A], a: A): String = schema match
    case Primitive.Boolean.Modify(self, _, g)  => print(schema = self, g(a))
    case Primitive.Boolean.Root                => String.valueOf(a)
    case Primitive.Number.BigDecimal(_)        => a.toPlainString
    case Primitive.Number.BigInteger(_)        => String.valueOf(a)
    case Primitive.Number.Double(_)            => String.valueOf(a)
    case Primitive.Number.Float(_)             => String.valueOf(a)
    case Primitive.Number.Int(_)               => String.valueOf(a)
    case Primitive.Number.Long(_)              => String.valueOf(a)
    case Primitive.Number.Modify(self, _, g)   => print(schema = self, g(a))
    case Primitive.String.Modify(self, _, g)   => print(schema = self, g(a))
    case Primitive.String.Parser(_, _, encode) => encode(a)
    case Primitive.String.Root(_)              => a
