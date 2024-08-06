package io.taig.otter

import cats.data.NonEmptyChain
import cats.syntax.all.*
import cats.Semigroup
import cats.data.NonEmptyMap
import cats.data.NonEmptyList
import cats.data.Chain

enum Violations:
  case Root(violations: NonEmptyChain[Violation])
  case Namespace(toNem: NonEmptyMap[Option[Step], Violations])

  final def /:(step: Step): Violations = Namespace(NonEmptyMap.one(step.some, this))
  final def /:(index: Int): Violations = /:(Step.Index(index))
  final def /:(field: String): Violations = /:(Step.Field(field))

  final def print: NonEmptyList[String] = print(history = Chain.nil)

  private def print(history: Chain[Step]): NonEmptyList[String] = this match
    case Root(violations) => violations.map(violation => s"${printHistory(history)}: $violation").toNonEmptyList
    case Namespace(toNem) =>
      toNem.toNel.flatMap:
        case (Some(step), violations) => violations.print(history :+ step)
        case (None, violations)       => violations.print(history)

  final override def toString: String = print.mkString_("\n")

object Violations:
  def of[A](violation: (Step, Violation), violations: (Step, Violation)*): Violations = Namespace(
    NonEmptyMap.of(violation, violations*).mapBoth { case (key, value) => (key.some, Root(NonEmptyChain.one(value))) }
  )

  def root(violations: NonEmptyChain[Violation]): Violations = Root(violations)
  def rootNec(violation: Violation): Violations = root(NonEmptyChain.one(violation))

  def namespace(step: Step, violations: NonEmptyChain[Violation]): Violations =
    Namespace(NonEmptyMap.one(step.some, Root(violations)))
  def namespaceNec(step: Step, violation: Violation): Violations =
    namespace(step, NonEmptyChain.one(violation))

  def parse(values: NonEmptyList[String]) = ???

  given Semigroup[Violations] with
    override def combine(x: Violations, y: Violations): Violations = (x, y) match
      case (Root(left), Root(right))           => Root(left.concat(right))
      case (Namespace(left), Namespace(right)) => Namespace(left |+| right)
      case (left @ Root(_), Namespace(right))  => Namespace(right |+| NonEmptyMap.one(none, left))
      case (Namespace(left), right @ Root(_))  => Namespace(left |+| NonEmptyMap.one(none, right))
