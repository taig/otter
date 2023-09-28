package io.taig.otter.validation

import cats.Semigroup
import cats.data.{Chain, NonEmptyChain, NonEmptyMap}
import cats.syntax.all.*

import scala.collection.immutable.SortedMap

opaque type Violations = NonEmptyMap[History, NonEmptyChain[Violation]]

object Violations:
  extension (violations: Violations)
    def toNem: NonEmptyMap[History, NonEmptyChain[Violation]] = violations
    def modifyHistory(f: History => History): Violations = violations.mapKeys(f)
    def modifyViolations(
        f: NonEmptyChain[Violation] => NonEmptyChain[Violation]
    ): Violations = violations.map(f)
    def modifyViolation(f: Violation => Violation): Violations = modifyViolations(_.map(f))
    infix def merge(right: Violations): Violations = violations |+| right
    def get(history: History): Chain[Violation] = violations.apply(history).map(_.toChain).orEmpty
    def head(history: History): Option[Violation] = violations.apply(history).map(_.head)

  def apply(violations: NonEmptyMap[History, NonEmptyChain[Violation]]): Violations = violations

  def of(head: (History, NonEmptyChain[Violation]), tail: (History, NonEmptyChain[Violation])*): Violations =
    NonEmptyMap.of(head, tail*)

  def ofNec(head: (History, Violation), tail: (History, Violation)*): Violations =
    NonEmptyMap.of(head.map(NonEmptyChain.one), tail.map(_.map(NonEmptyChain.one))*)

  def one(history: History, violations: NonEmptyChain[Violation]): Violations =
    NonEmptyMap.one(history, violations)

  def oneNec(history: History, violation: Violation): Violations = one(history, NonEmptyChain.one(violation))

  def root(violations: NonEmptyChain[Violation]): Violations = one(History.Root, violations)

  def rootNec(violation: Violation): Violations = oneNec(History.Root, violation)

  def fromMap(values: SortedMap[History, NonEmptyChain[Violation]]): Option[Violations] = NonEmptyMap.fromMap(values)

  given (using semigroup: Semigroup[NonEmptyMap[History, NonEmptyChain[Violation]]]): Semigroup[Violations] =
    semigroup
