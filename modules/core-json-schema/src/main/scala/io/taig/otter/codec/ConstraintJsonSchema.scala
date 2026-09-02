package io.taig.otter.codec

import cats.data.Chain
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.data.circe.toJson
import io.taig.otter.Constraint
import io.taig.validation.Comparison

/** Turns the constraints a schema's [[io.taig.validation.Validation]] carries into JSON Schema keywords.
  *
  * A `Validation` is introspectable: `and` concatenates `constraints`, so what a primitive was built with is still
  * there to be read. Not every constraint has a counterpart -- JSON Schema has no way to say that a collection is
  * sorted -- and rather than approximate one, those render as nothing. A generated schema that validates less than the
  * decoder does is safe; one that validates something else is not.
  *
  * Two places where the vocabularies do not line up, and the answer is to say the weaker thing rather than the wrong
  * one. [[Constraint.Primitive.Text.Matches]] is a full match and `pattern` is unanchored, so the pattern is anchored
  * here. And a length is counted in UTF-16 code units by the validation and in code points by JSON Schema, so a string
  * holding an astral character counts one more for the decoder than for a validator -- there is nothing to be done
  * about that in a keyword, and it is the safe direction only for a maximum.
  */
object ConstraintJsonSchema:
  def keywords(constraints: Chain[Constraint]): List[(String, CirceJson)] = constraints.toList.flatMap(keyword)

  def keyword(constraint: Constraint): Option[(String, CirceJson)] = constraint match
    case Constraint.Primitive.Text.Minimum(comparison) => count("minLength", comparison, offset = 1).some
    case Constraint.Primitive.Text.Maximum(comparison) => count("maxLength", comparison, offset = -1).some
    case Constraint.Primitive.Text.Matches(reference)  => ("pattern" -> CirceJson.fromString(anchor(reference))).some
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
    val reference = if comparison.exclusive then comparison.reference + offset else comparison.reference
    name -> CirceJson.fromLong(reference)

  /** A pattern that has to match the whole string, said the way JSON Schema says it.
    *
    * `pattern` is unanchored, and [[Constraint.Primitive.Text.Matches]] is `matches`, which is not. The group is what
    * keeps a top level alternation inside the anchors: `^a|b$` is not `^(?:a|b)$`.
    *
    * What is emitted is a `java.util.regex` source string in a slot JSON Schema says is ECMA-262. The two agree on
    * everything a schema is likely to say and disagree about `\p{...}`, possessive quantifiers and inline flags; there
    * is no way to check that at render time, so the assumption is stated rather than enforced.
    */
  private def anchor(pattern: java.util.regex.Pattern): String = s"^(?:${pattern.pattern()})$$"
