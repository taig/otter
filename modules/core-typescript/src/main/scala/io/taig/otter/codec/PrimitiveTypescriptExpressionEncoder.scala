package io.taig.otter.codec

import io.taig.otter.Primitive

import scala.annotation.tailrec
import io.taig.otter.Typescript
import java.math.BigDecimal as JBigDecimal

final class PrimitiveTypescriptExpressionEncoder[F[_]](encoder: => Encoder[F, Typescript.Expression])
    extends Encoder[Primitive.Write[F, *], Typescript.Expression]:
  @tailrec
  override def encode[A](schema: Primitive.Write[F, A], a: A): Typescript.Expression = schema match
    case Primitive.Boolean.Modify(self, _, f)           => encode(schema = self, f(a))
    case Primitive.Boolean.Root                         => Typescript.Expression.Literal.Boolean(a)
    case Primitive.Boolean.Write.Modify(self, f)        => encode(schema = self, f(a))
    case Primitive.Coerce.Boolean.Modify(self, _, f)    => encode(schema = self, f(a))
    case Primitive.Coerce.Boolean.Root(schema)          => encoder.encode(schema.value, a)
    case Primitive.Coerce.Boolean.Write.Modify(self, f) => encode(schema = self, f(a))
    case Primitive.Coerce.Modify(self, _, f)            => encode(schema = self, f(a))
    case Primitive.Coerce.Number.Modify(self, _, f)     => encode(schema = self, f(a))
    case Primitive.Coerce.Number.Root(schema)           => encoder.encode(schema.value, a)
    case Primitive.Coerce.Number.Write.Modify(self, f)  => encode(schema = self, f(a))
    case Primitive.Coerce.Text.Modify(self, _, f)       => encode(schema = self, f(a))
    case Primitive.Coerce.Text.Root(schema)             => encoder.encode(schema.value, a)
    case Primitive.Coerce.Text.Write.Modify(self, f)    => encode(schema = self, f(a))
    case Primitive.Coerce.Write.Modify(self, f)         => encode(schema = self, f(a))
    case Primitive.Number.BigDecimal(_)                 => Typescript.Expression.Literal.Number(a)
    case Primitive.Number.BigInteger(_)                 => Typescript.Expression.Literal.Number(new JBigDecimal(a))
    case Primitive.Number.Double(_)                     => Typescript.Expression.Literal.Number(new JBigDecimal(a))
    case Primitive.Number.Float(_)              => Typescript.Expression.Literal.Number(new JBigDecimal(a.toDouble))
    case Primitive.Number.Int(_)                => Typescript.Expression.Literal.Number(new JBigDecimal(a))
    case Primitive.Number.Long(_)               => Typescript.Expression.Literal.Number(new JBigDecimal(a))
    case Primitive.Number.Modify(self, _, f)    => encode(schema = self, f(a))
    case Primitive.Number.Write.Modify(self, f) => encode(schema = self, f(a))
    case Primitive.Text.Codec(_, _, print)      => Typescript.Expression.Literal.String(print(a))
    case Primitive.Text.Modify(self, _, f)      => encode(schema = self, f(a))
    case Primitive.Text.Root(_)                 => Typescript.Expression.Literal.String(a)
    case Primitive.Text.Write.Modify(self, f)   => encode(schema = self, f(a))
    case Primitive.Text.Write.Printer(_, print) => Typescript.Expression.Literal.String(print(a))
