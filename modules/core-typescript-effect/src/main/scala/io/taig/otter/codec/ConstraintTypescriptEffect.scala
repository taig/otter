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

/** Turns the constraints a schema's [[io.taig.validation.Validation]] carries into the filters effect pipes a schema
  * through.
  *
  * A `Validation` is introspectable: `and` concatenates `constraints`, so what a primitive was built with is still
  * there to be read. Not every constraint has a counterpart -- effect has no filter for a unique or sorted collection
  * or for the size of a record -- and rather than approximate one, those render as nothing. A generated schema that
  * validates less than the server does is safe; one that validates something else is not.
  *
  * A pattern is the one constraint whose counterpart is only sometimes there, because a `java.util.regex` pattern is
  * not a JavaScript one. [[TypescriptRegex]] is what decides, and says nothing where the two would disagree.
  */
object ConstraintTypescriptEffect:
  def filters(constraints: Chain[Constraint]): List[Typescript.Expression] =
    constraints.toList.flatMap(filter)

  def filter(constraint: Constraint): Option[Typescript.Expression] = constraint match
    case Constraint.Primitive.Text.Minimum(comparison) => length("minLength", comparison, offset = 1).some
    case Constraint.Primitive.Text.Maximum(comparison) => length("maxLength", comparison, offset = -1).some
    case Constraint.Primitive.Text.Matches(reference)  =>
      TypescriptRegex(reference.pattern()).map(TypescriptEffect.filter("pattern", _))
    case Constraint.Primitive.Number.Minimum(Comparison(reference, true))  => bound("greaterThan", reference).some
    case Constraint.Primitive.Number.Minimum(Comparison(reference, false)) =>
      bound("greaterThanOrEqualTo", reference).some
    case Constraint.Primitive.Number.Maximum(Comparison(reference, true))  => bound("lessThan", reference).some
    case Constraint.Primitive.Number.Maximum(Comparison(reference, false)) => bound("lessThanOrEqualTo", reference).some
    case Constraint.Primitive.Number.Multiple(reference)                   => bound("multipleOf", reference).some
    case Constraint.Collection.Minimum(comparison) => length("minItems", comparison, offset = 1).some
    case Constraint.Collection.Maximum(comparison) => length("maxItems", comparison, offset = -1).some
    case Constraint.Collection.Unique              => none
    case _: Constraint.Collection.Sorted           => none
    case _: Constraint.Object                      => none
    case _: Constraint.Generic                     => none

  /** The rest of the constraints, when they say the collection is never empty.
    *
    * `Schema.Array(x).pipe(Schema.minItems(1))` and `Schema.NonEmptyArray(x)` accept the same documents, and only the
    * second one types as `readonly [T, ...T[]]`. Which is the whole reason to tell them apart: a caller that has to
    * check whether the first element is there has lost something the schema already knew. The minimum is taken out of
    * what is left, because the array now says it.
    */
  def nonEmpty(constraints: Chain[Constraint]): Option[Chain[Constraint]] =
    Option.when(constraints.exists(isNonEmpty))(constraints.filterNot(isNonEmpty))

  private val isNonEmpty: Constraint => Boolean =
    case Constraint.Collection.Minimum(comparison) => inclusive(comparison, offset = 1) == 1
    case _                                         => false

  /** An exclusive bound on a length is the inclusive one next to it, because a length is an integer. */
  private def inclusive(comparison: Comparison[Long], offset: Long): Long =
    if comparison.exclusive then comparison.reference + offset else comparison.reference

  private def length(name: String, comparison: Comparison[Long], offset: Long): Typescript.Expression =
    TypescriptEffect.filter(name, TypescriptEffect.number(new JBigDecimal(inclusive(comparison, offset))))

  private def bound(name: String, reference: Data.Number): Typescript.Expression =
    TypescriptEffect.filter(name, TypescriptEffect.number(decimal(reference)))

  private def decimal(value: Data.Number): JBigDecimal = value match
    case value: JBigDecimal => value
    case value: JBigInteger => new JBigDecimal(value)
    case value: Long        => new JBigDecimal(value)
    case value: Int         => new JBigDecimal(value)
    case value: Float       => new JBigDecimal(value.toDouble)
    case value: Double      => new JBigDecimal(value)
