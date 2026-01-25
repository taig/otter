package io.taig.otter.codec

import io.taig.otter.Primitive
import io.taig.otter.Typescript

import java.math.BigDecimal as JBigDecimal
import scala.annotation.tailrec

object PrimitiveTypescriptTypeLiteralEncoder extends Encoder[Primitive.Write, Typescript.Type.Literal]:
  @tailrec
  override def encode[A](schema: Primitive.Write[A], a: A): Typescript.Type.Literal = schema match
    case Primitive.Boolean.Modify(self, _, f)    => encode(schema = self, f(a))
    case Primitive.Boolean.Root                  => Typescript.Type.Literal.Boolean(a)
    case Primitive.Boolean.Write.Modify(self, f) => encode(schema = self, f(a))
    case Primitive.Modify(self, _, f)            => encode(schema = self, f(a))
    case Primitive.Number.BigDecimal(_)          => Typescript.Type.Literal.Number(a)
    case Primitive.Number.BigInteger(_)          => Typescript.Type.Literal.Number(new JBigDecimal(a))
    case Primitive.Number.Double(_)              => Typescript.Type.Literal.Number(new JBigDecimal(a))
    case Primitive.Number.Float(_)               => Typescript.Type.Literal.Number(new JBigDecimal(a.toDouble))
    case Primitive.Number.Int(_)                 => Typescript.Type.Literal.Number(new JBigDecimal(a))
    case Primitive.Number.Long(_)                => Typescript.Type.Literal.Number(new JBigDecimal(a))
    case Primitive.Number.Modify(self, _, f)     => encode(schema = self, f(a))
    case Primitive.Number.Write.Modify(self, f)  => encode(schema = self, f(a))
    case Primitive.Text.Codec(_, _, print)       => Typescript.Type.Literal.String(print(a))
    case Primitive.Text.Modify(self, _, f)       => encode(schema = self, f(a))
    case Primitive.Text.Root(_)                  => Typescript.Type.Literal.String(a)
    case Primitive.Text.Write.Modify(self, f)    => encode(schema = self, f(a))
    case Primitive.Text.Write.Printer(_, print)  => Typescript.Type.Literal.String(print(a))
