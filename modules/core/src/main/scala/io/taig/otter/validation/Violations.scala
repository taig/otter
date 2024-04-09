package io.taig.otter.validation

import cats.Semigroup
import cats.data.{Chain, NonEmptyChain, NonEmptyMap}
import cats.syntax.all.*

import scala.collection.immutable.SortedMap

opaque type Violations[+C] = NonEmptyMap[History, NonEmptyChain[Violation[C]]]

object Violations:
//   extension (violations: Violations)
//     def toNem: NonEmptyMap[History, NonEmptyChain[Violation]] = violations
//     def modifyHistory(f: History => History): Violations = violations.mapKeys(f)
//     def modifyViolations(
//         f: NonEmptyChain[Violation] => NonEmptyChain[Violation]
//     ): Violations = violations.map(f)
//     def modifyViolation(f: Violation => Violation): Violations = modifyViolations(_.map(f))
//     infix def merge(right: Violations): Violations = violations |+| right
//     def get(history: History): Chain[Violation] = violations.apply(history).map(_.toChain).orEmpty
//     def head(history: History): Option[Violation] = violations.apply(history).map(_.head)

  def apply[C](violations: NonEmptyMap[History, NonEmptyChain[Violation[C]]]): Violations[C] = violations

//   def of(head: (History, NonEmptyChain[Violation]), tail: (History, NonEmptyChain[Violation])*): Violations =
//     NonEmptyMap.of(head, tail*)

//   def ofNec(head: (History, Violation), tail: (History, Violation)*): Violations =
//     NonEmptyMap.of(head.map(NonEmptyChain.one), tail.map(_.map(NonEmptyChain.one))*)

  def one[C](history: History, violations: NonEmptyChain[Violation[C]]): Violations[C] =
    NonEmptyMap.one(history, violations)

  def oneNec[C](history: History, violation: Violation[C]): Violations[C] =
    one[C](history, NonEmptyChain.one(violation))

  def root[C](violations: NonEmptyChain[Violation[C]]): Violations[C] = one(History.Root, violations)

  def rootNec[C](violation: Violation[C]): Violations[C] = oneNec(History.Root, violation)

//   def fromMap(values: SortedMap[History, NonEmptyChain[Violation]]): Option[Violations] = NonEmptyMap.fromMap(values)

//   given (using semigroup: Semigroup[NonEmptyMap[History, NonEmptyChain[Violation]]]): Semigroup[Violations] =
//     semigroup
