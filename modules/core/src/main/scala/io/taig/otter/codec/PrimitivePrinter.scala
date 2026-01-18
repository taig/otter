package io.taig.otter.codec

import io.taig.otter.Primitive

import scala.annotation.tailrec

final class PrimitivePrinter[F[_]](printer: Printer[F]) extends Printer[Primitive.Write[F, *]]:
  @tailrec
  override def encode[A](schema: Primitive.Write[F, A], a: A): String = schema match
    case Primitive.Boolean.Modify(self, _, f)           => encode(schema = self, f(a))
    case Primitive.Boolean.Root                         => String.valueOf(a)
    case Primitive.Boolean.Write.Modify(self, f)        => encode(schema = self, f(a))
    case Primitive.Coerce.Boolean.Modify(self, _, f)    => encode(schema = self, f(a))
    case Primitive.Coerce.Boolean.Root(schema)          => printer.encode(schema.value, a)
    case Primitive.Coerce.Boolean.Write.Modify(self, f) => encode(schema = self, f(a))
    case Primitive.Coerce.Modify(self, _, f)            => encode(schema = self, f(a))
    case Primitive.Coerce.Number.Modify(self, _, f)     => encode(schema = self, f(a))
    case Primitive.Coerce.Number.Root(schema)           => printer.encode(schema.value, a)
    case Primitive.Coerce.Number.Write.Modify(self, f)  => encode(schema = self, f(a))
    case Primitive.Coerce.Text.Modify(self, _, f)       => encode(schema = self, f(a))
    case Primitive.Coerce.Text.Root(schema)             => printer.encode(schema.value, a)
    case Primitive.Coerce.Text.Write.Modify(self, f)    => encode(schema = self, f(a))
    case Primitive.Coerce.Write.Modify(self, f)         => encode(schema = self, f(a))
    case Primitive.Number.BigDecimal(_)                 => a.toPlainString
    case Primitive.Number.BigInteger(_)                 => String.valueOf(a)
    case Primitive.Number.Double(_)                     => String.valueOf(a)
    case Primitive.Number.Float(_)                      => String.valueOf(a)
    case Primitive.Number.Int(_)                        => String.valueOf(a)
    case Primitive.Number.Long(_)                       => String.valueOf(a)
    case Primitive.Number.Modify(self, _, f)            => encode(schema = self, f(a))
    case Primitive.Number.Write.Modify(self, f)         => encode(schema = self, f(a))
    case Primitive.Text.Codec(_, _, print)              => print(a)
    case Primitive.Text.Modify(self, _, f)              => encode(schema = self, f(a))
    case Primitive.Text.Root(_)                         => a
    case Primitive.Text.Write.Modify(self, f)           => encode(schema = self, f(a))
    case Primitive.Text.Write.Printer(_, print)         => print(a)
