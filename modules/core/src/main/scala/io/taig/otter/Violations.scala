package io.taig.otter

import cats.data.NonEmptyChain
import cats.Semigroup
import cats.data.NonEmptyMap
import cats.data.NonEmptyList
import cats.parse.Parser
import cats.data.Chain
import scala.collection.immutable.SortedMap
import cats.implicits.*
import cats.Show

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

  final def toNem: NonEmptyMap[XPath, NonEmptyChain[Violation]] = toNem(xpath = XPath.Empty)

  private def toNem(xpath: XPath): NonEmptyMap[XPath, NonEmptyChain[Violation]] = this match
    case Root(values, violations) =>
      val root = NonEmptyMap.one(xpath, violations)

      NonEmptyList
        .fromList(values.toList)
        .map(_.map { case (step, violations) => violations.toNem(xpath / step) }.reduce)
        .fold(root)(root.combine)
    case Namespace(values) =>
      values.toNel.map { case (step, violations) => violations.toNem(xpath / step) }.reduce

  final def toNel: NonEmptyList[Indexed[NonEmptyChain[Violation]]] = toNem.toNel.map(Indexed.apply)

  final override def toString: String = Printers(this).mkString_("\n")

object Violations:
  def root(violations: NonEmptyChain[Violation]): Violations = Root(values = SortedMap.empty, violations)
  def rootNec(violation: Violation): Violations = root(NonEmptyChain.one(violation))

  def of[A](violation: (Step, Violation), violations: (Step, Violation)*): Violations = Namespace(
    NonEmptyMap.of(violation, violations*).mapBoth { case (step, violation) => (step, rootNec(violation)) }
  )

  def namespace(step: Step, violations: NonEmptyChain[Violation]): Violations =
    Namespace(NonEmptyMap.one(step, root(violations)))
  def namespaceNec(step: Step, violation: Violation): Violations = namespace(step, NonEmptyChain.one(violation))

  def from(violations: Indexed[NonEmptyChain[Violation]]): Violations =
    violations.xpath.toChain.foldRight(root(violations.self))(_ /: _)

  def from(values: NonEmptyMap[XPath, NonEmptyChain[Violation]]): Violations = from(values.toNel.map(Indexed.apply))

  def from(values: NonEmptyList[Indexed[NonEmptyChain[Violation]]]): Violations =
    val NonEmptyList(head, tail) = values.map(from)
    tail.foldLeft(head)(_.combine(_))

  def parse(values: NonEmptyList[String]): Either[Parser.Error, Violations] = NonEmptyChain
    .fromNonEmptyList(values)
    .traverse(Parsers.indexed(Parsers.violation).parseAll)
    .map(_.groupMapNem(_.xpath)(_.self))
    .map(from)

  // TODO parse properly
  def parse(value: String): Either[Parser.Error, Violations] =
    parse(NonEmptyList.fromList(value.split('\n').toList).getOrElse(NonEmptyList.one("")))

  given Semigroup[Violations] with
    override def combine(x: Violations, y: Violations): Violations = x.combine(y)

  given Show[Violations] = Show.fromToString
