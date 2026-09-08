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
  *
  * A binary float reaches the literal through its own `toString` rather than through `new BigDecimal(double)`. The
  * constructor is exact -- it spells the binary value in full, so `0.1` becomes
  * `0.1000000000000000055511151231257827021181583404541015625` -- and a literal spelled that way denotes a value no
  * JSON encoder ever writes, so the generated schema would reject the very document this library produced. `toString`
  * is the shortest text that reads back as the same value, which is what a JSON writer spells too, and it tracks the
  * platform: Scala.js has no float formatting of its own, and there both this and the document say
  * `0.10000000149011612`.
  */
object PrimitiveTypescriptExpressionLiteralEncoder extends Encoder[Primitive, Typescript.Expression.Literal]:
  @tailrec
  override def encode[W](schema: Primitive[W, Any], w: W): Typescript.Expression.Literal = schema match
    case Primitive.Modify(self, _, g)         => encode(self, g(w))
    case Primitive.Boolean.Modify(self, _, g) => encode(self, g(w))
    case Primitive.Boolean.Root               => Typescript.Expression.Literal.Boolean(w)
    case Primitive.Number.BigDecimal(_)       => Typescript.Expression.Literal.Number(w)
    case Primitive.Number.BigInteger(_)       => Typescript.Expression.Literal.Number(new JBigDecimal(w))
    case Primitive.Number.Double(_)           => Typescript.Expression.Literal.Number(JBigDecimal.valueOf(w))
    case Primitive.Number.Float(_)            => Typescript.Expression.Literal.Number(new JBigDecimal(w.toString))
    case Primitive.Number.Int(_)              => Typescript.Expression.Literal.Number(new JBigDecimal(w))
    case Primitive.Number.Long(_)             => Typescript.Expression.Literal.Number(new JBigDecimal(w))
    case Primitive.Number.Modify(self, _, g)  => encode(self, g(w))
    case Primitive.Text.Format(_, _, print)   => Typescript.Expression.Literal.String(print(w))
    case Primitive.Text.Modify(self, _, g)    => encode(self, g(w))
    case Primitive.Text.Root(_)               => Typescript.Expression.Literal.String(w)
