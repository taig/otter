package io.taig.otter

import cats.Semigroup
import cats.data.NonEmptyChain
import cats.data.NonEmptyMap
import cats.implicits.*
import io.taig.validation.Violation

import scala.collection.immutable.SortedMap

/** A tree of constraint violations, indexed by the [[Step]] path at which they occurred. */
enum Violations:
  case Root(values: SortedMap[Step, Violations], violations: NonEmptyChain[Violation[Constraint]])
  case Namespace(values: NonEmptyMap[Step, Violations])

  final def /:(step: Step): Violations = Namespace(NonEmptyMap.one(step, this))
  final def /:(index: Int): Violations = /:(Step.Index(index))
  final def /:(field: String): Violations = /:(Step.Field(field))

  /** Both trees, with the nodes under a shared [[Step]] combined rather than one of them chosen.
    *
    * `|+|` in all four cases, and the two mixed ones are why this is spelled out: a `SortedMap.++` would keep whichever
    * side it was given second and drop the other's whole subtree, so a node reporting violations of its own beside a
    * failing child would lose that child as soon as a sibling failed at the same step. Nothing here may drop a
    * violation -- [[io.taig.otter.JsonCirce.failures]] promises the caller every one of them.
    */
  final def combine(violations: Violations): Violations = (this, violations) match
    case (left: Root, right: Root)           => Root(left.values |+| right.values, left.violations ++ right.violations)
    case (Namespace(left), Namespace(right)) => Namespace(left |+| right)
    case (left: Root, Namespace(right))      => Root(left.values |+| right.toSortedMap, left.violations)
    case (Namespace(left), right: Root)      => Root(left.toSortedMap |+| right.values, right.violations)

object Violations:
  def apply(violations: NonEmptyChain[Violation[Constraint]]): Violations = Root(values = SortedMap.empty, violations)

  def apply(violation: Violation[Constraint]): Violations = Violations(violations = NonEmptyChain.one(violation))

  given Semigroup[Violations] = (x, y) => x.combine(y)
