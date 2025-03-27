package io.taig.otter

import cats.Eq
import cats.Show
import cats.syntax.all.*
import cats.derived.strict.*
import cats.parse.Parser
import java.util.regex.Pattern

final case class Violation(constraint: Constraint, actual: Data.Any, hint: Option[String]) derives Eq:
  override def toString: String = ??? // Printers(this)

object Violation:
  def tpe(name: String, actual: Data.Any): Violation = Violation(Constraint.Type(name), actual, hint = none)
  def tpe(name: String, actual: Data.Any, hint: String): Violation =
    Violation(Constraint.Type(name), actual, hint = hint.some)

  def oneOf(values: List[Data.Primitive], actual: Data.Any): Violation =
    Violation(Constraint.OneOf(values), actual, hint = none)

  def matches(pattern: Pattern, actual: Data.Any): Violation =
    Violation(Constraint.Primitive.Matches(pattern), actual, hint = none)
  def matches(expected: String, actual: Data.Any): Violation =
    matches(pattern = Pattern.compile(Pattern.quote(expected)), actual)

  def parse(value: String): Either[Parser.Error, Violation] = ??? // Parsers.violation.parseAll(value)

  given Show[Violation] = Show.fromToString
