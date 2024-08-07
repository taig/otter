package io.taig.otter

import cats.data.NonEmptyChain
import cats.Semigroup
import cats.data.NonEmptyMap
import cats.data.NonEmptyList
import cats.parse.Parser
import cats.data.Chain
import scala.collection.immutable.SortedMap
import cats.implicits.*

enum Violations:
  case Root(values: SortedMap[Step, Violations], violations: NonEmptyChain[Violation])
  case Namespace(values: NonEmptyMap[Step, Violations])

  final def /:(step: Step): Violations = Namespace(NonEmptyMap.one(step, this))
  final def /:(index: Int): Violations = /:(Step.Index(index))
  final def /:(field: String): Violations = /:(Step.Field(field))

  final def combine(violations: Violations): Violations = (this, violations) match
    case (left: Root, right: Root)           => Root(left.values |+| right.values, left.violations ++ right.violations)
    case (Namespace(left), Namespace(right)) => Namespace(left |+| right)
    case (left: Root, Namespace(right))      => Root(left.values |+| right.toSortedMap, left.violations)
    case (Namespace(left), right: Root)      => Root(left.toSortedMap |+| right.values, right.violations)

  final def toNem: NonEmptyMap[Chain[Step], NonEmptyChain[Violation]] = toNem(history = Chain.empty)

  private def toNem(history: Chain[Step]): NonEmptyMap[Chain[Step], NonEmptyChain[Violation]] = this match
    case Root(values, violations) =>
      val root = NonEmptyMap.one(history, violations)

      NonEmptyList
        .fromList(values.toList)
        .map(_.map { case (step, violations) => violations.toNem(history :+ step) }.reduce)
        .fold(root)(root.combine)
    case Namespace(values) =>
      values.toNel.map { case (step, violations) => violations.toNem(history :+ step) }.reduce

  final def print: NonEmptyList[String] = Printers(this)
  final override def toString: String = print.mkString_("\n")

object Violations:
  def root(violations: NonEmptyChain[Violation]): Violations = Root(values = SortedMap.empty, violations)
  def rootNec(violation: Violation): Violations = root(NonEmptyChain.one(violation))

  def of[A](violation: (Step, Violation), violations: (Step, Violation)*): Violations = Namespace(
    NonEmptyMap.of(violation, violations*).mapBoth { case (step, violation) => (step, rootNec(violation)) }
  )

  def namespace(step: Step, violations: NonEmptyChain[Violation]): Violations =
    Namespace(NonEmptyMap.one(step, root(violations)))
  def namespaceNec(step: Step, violation: Violation): Violations =
    namespace(step, NonEmptyChain.one(violation))

  def from(nem: NonEmptyMap[Chain[Step], NonEmptyChain[Violation]]): Violations = ???

  def parse(values: NonEmptyList[String]): Either[Parser.Error, Violations] = values
    .traverse(Parsers.violations.parseAll)
    .map(_.groupMapNem { case (history, _) => history } { case (_, violation) => violation })
    .map(_.map(NonEmptyChain.fromNonEmptyList))
    .map(from)

  given Semigroup[Violations] with
    override def combine(x: Violations, y: Violations): Violations = x.combine(y)
