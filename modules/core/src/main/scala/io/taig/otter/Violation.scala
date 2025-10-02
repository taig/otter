package io.taig.otter

import cats.Eq
import cats.Show
import cats.data.NonEmptyChain
import cats.derived.*
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Constraint

final case class Violation(causes: NonEmptyChain[Violation.Cause], actual: Data) derives Eq:
  override def toString: String =
    val tail = causes.tail.map(cause => show"  $cause").mkString_("\n")

    show"""! ${causes.head}${if tail.isEmpty then "" else s"\n$tail"}
          |> $actual""".stripMargin

object Violation:
  final case class Cause(constraint: Constraint, hint: Option[String]) derives Eq:
    override def toString: String = hint match
      case Some(hint) => show"$constraint [$hint]"
      case None       => show"$constraint"

  object Cause:
    given Show[Violation.Cause] = Show.fromToString

  def fromConstraint(constraint: Constraint, actual: Data, hint: Option[String] = none): Violation =
    Violation(causes = NonEmptyChain.one(Cause(constraint, hint)), actual)

  def fromConstraints(constraints: NonEmptyChain[Constraint], actual: Data): Violation =
    Violation(causes = constraints.map(Cause(_, hint = none)), actual)

  given Show[Violation] = Show.fromToString
