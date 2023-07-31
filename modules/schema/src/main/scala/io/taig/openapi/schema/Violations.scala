package io.taig.openapi.schema

import cats.Semigroup
import cats.data.{Chain, NonEmptyChain, NonEmptyMap}
import cats.syntax.all.*
import io.taig.openapi.validation.Violation

import scala.collection.immutable.SortedMap

opaque type Violations[Act] = NonEmptyMap[History, NonEmptyChain[Violation[Act]]]

object Violations:
  extension [Act](violations: Violations[Act])
    def toNem: NonEmptyMap[History, NonEmptyChain[Violation[Act]]] = violations
    def modifyHistory(f: History => History): Violations[Act] = violations.mapKeys(f)
    def modifyViolations(
        f: NonEmptyChain[Violation[Act]] => NonEmptyChain[Violation[Act]]
    ): Violations[Act] = violations.map(f)
    def modifyViolation(f: Violation[Act] => Violation[Act]): Violations[Act] =
      modifyViolations(_.map(f))
    infix def merge(right: Violations[Act]): Violations[Act] = violations |+| right
    def get(history: History): Chain[Violation[Act]] = violations.apply(history).map(_.toChain).orEmpty
    def head(history: History): Option[Violation[Act]] = violations.apply(history).map(_.head)

  def apply[Act](violations: NonEmptyMap[History, NonEmptyChain[Violation[Act]]]): Violations[Act] = violations

  def of[Act](
      head: (History, NonEmptyChain[Violation[Act]]),
      tail: (History, NonEmptyChain[Violation[Act]])*
  ): Violations[Act] =
    NonEmptyMap.of(head, tail*)

  def ofNec[Act](head: (History, Violation[Act]), tail: (History, Violation[Act])*): Violations[Act] =
    NonEmptyMap.of(head.map(NonEmptyChain.one), tail.map(_.map(NonEmptyChain.one))*)

  def one[Act](history: History, violations: NonEmptyChain[Violation[Act]]): Violations[Act] =
    NonEmptyMap.one(history, violations)

  def oneNec[Act](history: History, violation: Violation[Act]): Violations[Act] =
    one(history, NonEmptyChain.one(violation))

  def root[Act](violations: NonEmptyChain[Violation[Act]]): Violations[Act] = one(History.Root, violations)

  def rootNec[Act](violation: Violation[Act]): Violations[Act] = oneNec(History.Root, violation)

  def fromMap[Act](values: SortedMap[History, NonEmptyChain[Violation[Act]]]): Option[Violations[Act]] =
    NonEmptyMap.fromMap(values)

  given [Act](using
      semigroup: Semigroup[NonEmptyMap[History, NonEmptyChain[Violation[Act]]]]
  ): Semigroup[Violations[Act]] = semigroup
