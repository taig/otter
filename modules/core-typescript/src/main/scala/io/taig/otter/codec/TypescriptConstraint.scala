package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.validation.Comparison

/** What the constraints on a schema say that a TypeScript target says with something other than a filter, whatever the
  * target is.
  */
object TypescriptConstraint:
  /** The rest of the constraints, when they say the collection is never empty.
    *
    * A minimum of one is the difference between an array and an array that has a first element, and both the value a
    * target emits and the type it declares have their own way of saying the second. The minimum is taken out of what is
    * left, because whatever says it says it instead of a filter.
    */
  def nonEmpty(constraints: Chain[Constraint]): Option[Chain[Constraint]] =
    Option.when(constraints.exists(isNonEmpty))(constraints.filterNot(isNonEmpty))

  def isNonEmpty(constraints: Chain[Constraint]): Boolean = constraints.exists(isNonEmpty)

  private val isNonEmpty: Constraint => Boolean =
    case Constraint.Collection.Minimum(comparison) => inclusive(comparison, offset = 1) == 1
    case _                                         => false

  /** An exclusive bound on a length is the inclusive one next to it, because a length is an integer. */
  def inclusive(comparison: Comparison[Long], offset: Long): Long =
    if comparison.exclusive then comparison.reference + offset else comparison.reference
