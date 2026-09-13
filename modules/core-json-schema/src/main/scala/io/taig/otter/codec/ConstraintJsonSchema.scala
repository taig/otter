package io.taig.otter.codec

import cats.data.Chain
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.data.circe.toJson
import io.taig.otter.Constraint
import io.taig.validation.Comparison

/** Translates constraints conservatively. UTF-16 lengths and unsupported regexes have no faithful keyword here. */
object ConstraintJsonSchema:
  def keywords(constraints: Chain[Constraint]): List[(String, CirceJson)] = constraints.toList.flatMap(keyword)

  def keyword(constraint: Constraint): Option[(String, CirceJson)] = constraint match
    case _: Constraint.Primitive.Text.Minimum         => none
    case _: Constraint.Primitive.Text.Maximum         => none
    case Constraint.Primitive.Text.Matches(reference) =>
      RegexPattern(reference).map(value => "pattern" -> CirceJson.fromString(value))
    case Constraint.Primitive.Number.Minimum(Comparison(reference, true)) =>
      ("exclusiveMinimum" -> reference.toJson).some
    case Constraint.Primitive.Number.Minimum(Comparison(reference, false)) => ("minimum" -> reference.toJson).some
    case Constraint.Primitive.Number.Maximum(Comparison(reference, true))  =>
      ("exclusiveMaximum" -> reference.toJson).some
    case Constraint.Primitive.Number.Maximum(Comparison(reference, false)) => ("maximum" -> reference.toJson).some
    case Constraint.Primitive.Number.Multiple(reference)                   => ("multipleOf" -> reference.toJson).some
    case Constraint.Collection.Minimum(comparison) => count("minItems", comparison, offset = 1).some
    case Constraint.Collection.Maximum(comparison) => count("maxItems", comparison, offset = -1).some
    case Constraint.Collection.Unique              => ("uniqueItems" -> CirceJson.True).some
    case _: Constraint.Collection.Sorted           => none
    case Constraint.Object.Minimum(comparison)     => count("minProperties", comparison, offset = 1).some
    case Constraint.Object.Maximum(comparison)     => count("maxProperties", comparison, offset = -1).some
    case _: Constraint.Generic                     => none

  /** An exclusive bound on a count is the inclusive one next to it, because a count is an integer. */
  private def count(name: String, comparison: Comparison[Long], offset: Long): (String, CirceJson) =
    val reference = BigInt(comparison.reference) + (if comparison.exclusive then offset else 0L)
    if reference < 0 && name.startsWith("max") then "not" -> CirceJson.obj()
    else name -> CirceJson.fromBigInt(reference.max(BigInt(0)))
