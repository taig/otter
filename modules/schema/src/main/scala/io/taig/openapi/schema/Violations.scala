//package io.taig.openapi.schema
//
//import cats.Semigroup
//import cats.data.{Chain, NonEmptyChain, NonEmptyMap}
//import cats.syntax.all.*
//import io.taig.openapi.{History, OpenApi}
//
//import scala.collection.immutable.SortedMap
//
//opaque type Violations = NonEmptyMap[History, NonEmptyChain[Violation[OpenApi, OpenApi]]]
//
//object Violations:
//  extension (violations: Violations)
//    def toNem: NonEmptyMap[History, NonEmptyChain[Violation[OpenApi, OpenApi]]] = violations
//    def modifyHistory(f: History => History): Violations = violations.mapKeys(f)
//    def modifyViolations(
//        f: NonEmptyChain[Violation[OpenApi, OpenApi]] => NonEmptyChain[Violation[OpenApi, OpenApi]]
//    ): Violations = violations.map(f)
//    def modifyViolation(f: Violation[OpenApi, OpenApi] => Violation[OpenApi, OpenApi]): Violations =
//      modifyViolations(_.map(f))
//    infix def merge(right: Violations): Violations = violations |+| right
//    def get(history: History): Chain[Violation[OpenApi, OpenApi]] = violations.apply(history).map(_.toChain).orEmpty
//    def head(history: History): Option[Violation[OpenApi, OpenApi]] = violations.apply(history).map(_.head)
//
//  def apply(violations: NonEmptyMap[History, NonEmptyChain[Violation[OpenApi, OpenApi]]]): Violations = violations
//
//  def of(
//      head: (History, NonEmptyChain[Violation[OpenApi, OpenApi]]),
//      tail: (History, NonEmptyChain[Violation[OpenApi, OpenApi]])*
//  ): Violations = NonEmptyMap.of(head, tail*)
//
//  def ofNec(head: (History, Violation[OpenApi, OpenApi]), tail: (History, Violation[OpenApi, OpenApi])*): Violations =
//    NonEmptyMap.of(head.map(NonEmptyChain.one), tail.map(_.map(NonEmptyChain.one))*)
//
//  def one(history: History, violations: NonEmptyChain[Violation[OpenApi, OpenApi]]): Violations =
//    NonEmptyMap.one(history, violations)
//
//  def oneNec(history: History, violation: Violation[OpenApi, OpenApi]): Violations =
//    one(history, NonEmptyChain.one(violation))
//
//  def root(violations: NonEmptyChain[Violation[OpenApi, OpenApi]]): Violations = one(History.Root, violations)
//
//  def rootNec(violation: Violation[OpenApi, OpenApi]): Violations = oneNec(History.Root, violation)
//
//  def fromMap(values: SortedMap[History, NonEmptyChain[Violation[OpenApi, OpenApi]]]): Option[Violations] =
//    NonEmptyMap.fromMap(values)
//
//  given (using
//      semigroup: Semigroup[NonEmptyMap[History, NonEmptyChain[Violation[OpenApi, OpenApi]]]]
//  ): Semigroup[Violations] = semigroup
