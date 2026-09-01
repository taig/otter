package io.taig.otter.codec

import io.taig.otter.Primitive
import io.taig.otter.Typescript

import java.math.BigDecimal as JBigDecimal
import scala.annotation.tailrec

/** Writes a primitive value as the TypeScript literal that denotes it.
  *
  * An [[Encoder]] whose output is source rather than a document, which is what lets a [[Constant]]'s value and an
  * [[Enumeration]]'s mapping reach the generator without the generator knowing anything about either: the value is
  * pushed through the very schema that describes it, exactly as the circe encoder pushes it through to a `Json`.
  */
object PrimitiveTypescriptExpressionLiteralEncoder extends Encoder[Primitive, Typescript.Expression.Literal]:
  @tailrec
  override def encode[W](schema: Primitive[W, Any], w: W): Typescript.Expression.Literal = schema match
    case Primitive.Modify(self, _, g)         => encode(self, g(w))
    case Primitive.Boolean.Modify(self, _, g) => encode(self, g(w))
    case Primitive.Boolean.Root               => Typescript.Expression.Literal.Boolean(w)
    case Primitive.Number.BigDecimal(_)       => Typescript.Expression.Literal.Number(w)
    case Primitive.Number.BigInteger(_)       => Typescript.Expression.Literal.Number(new JBigDecimal(w))
    case Primitive.Number.Double(_)           => Typescript.Expression.Literal.Number(new JBigDecimal(w))
    case Primitive.Number.Float(_)            => Typescript.Expression.Literal.Number(new JBigDecimal(w.toDouble))
    case Primitive.Number.Int(_)              => Typescript.Expression.Literal.Number(new JBigDecimal(w))
    case Primitive.Number.Long(_)             => Typescript.Expression.Literal.Number(new JBigDecimal(w))
    case Primitive.Number.Modify(self, _, g)  => encode(self, g(w))
    case Primitive.Text.Format(_, _, print)   => Typescript.Expression.Literal.String(print(w))
    case Primitive.Text.Modify(self, _, g)    => encode(self, g(w))
    case Primitive.Text.Root(_)               => Typescript.Expression.Literal.String(w)
