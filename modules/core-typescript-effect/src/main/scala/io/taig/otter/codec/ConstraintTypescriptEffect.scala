package io.taig.otter.codec

import cats.data.Chain
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Constraint
import io.taig.otter.Typescript
import io.taig.otter.TypescriptEffect
import io.taig.validation.Comparison

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

/** Turns the constraints a schema's [[io.taig.validation.Validation]] carries into Effect v4 schema checks.
  *
  * A `Validation` is introspectable: `and` concatenates `constraints`, so what a primitive was built with is still
  * there to be read. Unique and sorted collections and record sizes have no supported translation here; rather than
  * approximate them, those render as nothing. A generated schema that validates less than the server does is safe; one
  * that validates something else is not.
  *
  * A pattern is the one constraint whose counterpart is only sometimes there, because a `java.util.regex` pattern is
  * not a JavaScript one. [[TypescriptRegex]] is what decides, and says nothing where the two would disagree.
  */
object ConstraintTypescriptEffect:
  def filters(constraints: Chain[Constraint]): List[Typescript.Expression] =
    constraints.toList.flatMap(filter)

  def filter(constraint: Constraint): Option[Typescript.Expression] = constraint match
    case Constraint.Primitive.Text.Minimum(comparison) => length("isMinLength", comparison, offset = 1).some
    case Constraint.Primitive.Text.Maximum(comparison) => maximumLength(comparison).some
    case Constraint.Primitive.Text.Matches(reference)  =>
      TypescriptRegex(reference).map(TypescriptEffect.filter("isPattern", _))
    case Constraint.Primitive.Number.Minimum(Comparison(reference, true))  => bound("isGreaterThan", reference).some
    case Constraint.Primitive.Number.Minimum(Comparison(reference, false)) =>
      bound("isGreaterThanOrEqualTo", reference).some
    case Constraint.Primitive.Number.Maximum(Comparison(reference, true))  => bound("isLessThan", reference).some
    case Constraint.Primitive.Number.Maximum(Comparison(reference, false)) =>
      bound("isLessThanOrEqualTo", reference).some
    case Constraint.Primitive.Number.Multiple(reference) => bound("isMultipleOf", reference).some
    case Constraint.Collection.Minimum(comparison)       =>
      Option.when(comparison.reference > 0 || (comparison.reference == 0 && comparison.exclusive))(
        length("isMinLength", comparison, offset = 1)
      )
    case Constraint.Collection.Maximum(comparison) => maximumLength(comparison).some
    case Constraint.Collection.Unique              => none
    case _: Constraint.Collection.Sorted           => none
    case _: Constraint.Object                      => none
    case _: Constraint.Generic                     => none

  private def maximumLength(comparison: Comparison[Long]): Typescript.Expression =
    if comparison.reference < 0 || (comparison.reference == 0 && comparison.exclusive) then
      TypescriptEffect.filter(
        "makeFilter",
        Typescript.Expression.Arrow(Nil, Typescript.Expression.Literal.Boolean(false))
      )
    else length("isMaxLength", comparison, offset = -1)

  private def length(name: String, comparison: Comparison[Long], offset: Long): Typescript.Expression =
    TypescriptEffect.filter(
      name,
      TypescriptEffect.number(
        JBigDecimal.valueOf(comparison.reference).add(JBigDecimal.valueOf(if comparison.exclusive then offset else 0L))
      )
    )

  private def bound(name: String, reference: Data.Number): Typescript.Expression =
    TypescriptEffect.filter(name, TypescriptEffect.number(decimal(reference)))

  /** The bound as a decimal, taking a binary float through its own `toString` for the reason
    * [[PrimitiveTypescriptExpressionLiteralEncoder]] does: `new BigDecimal(double)` is exact, so a bound of `0.1` would
    * be generated as `Schema.isGreaterThan(0.1000000000000000055511151231257827021181583404541015625)` and reject a
    * value the schema it was read from accepts.
    */
  private def decimal(value: Data.Number): JBigDecimal = value match
    case value: JBigDecimal => value
    case value: JBigInteger => new JBigDecimal(value)
    case value: Long        => new JBigDecimal(value)
    case value: Int         => new JBigDecimal(value)
    case value: Float       => new JBigDecimal(value.toString)
    case value: Double      => JBigDecimal.valueOf(value)
