package io.taig.otter

import scala.collection.immutable.SortedMap
import cats.data.NonEmptyChain
import cats.data.NonEmptyMap
import io.taig.data.Data
import io.taig.validation.Violation
import cats.implicits.*
import cats.Semigroup

enum Violations:
  case Root(values: SortedMap[Step, Violations], violations: NonEmptyChain[Violation[Constraint]])
  case Namespace(values: NonEmptyMap[Step, Violations])

  final def /:(step: Step): Violations = Namespace(NonEmptyMap.one(step, this))
  final def /:(index: Int): Violations = /:(Step.Index(index))
  final def /:(field: String): Violations = /:(Step.Field(field))

  final def combine(violations: Violations): Violations = (this, violations) match
    case (left: Root, right: Root)           => Root(left.values |+| right.values, left.violations ++ right.violations)
    case (Namespace(left), Namespace(right)) => Namespace(left |+| right)
    case (left: Root, Namespace(right))      => Root(right.toSortedMap ++ left.values, left.violations)
    case (Namespace(left), right: Root)      => Root(left.toSortedMap ++ right.values, right.violations)

object Violations:
  def apply(violations: NonEmptyChain[Violation[Constraint]]): Violations = Root(values = SortedMap.empty, violations)

  def apply(violation: Violation[Constraint]): Violations = Violations(violations = NonEmptyChain.one(violation))

  given Semigroup[Violations] with
    def combine(x: Violations, y: Violations): Violations = x.combine(y)
