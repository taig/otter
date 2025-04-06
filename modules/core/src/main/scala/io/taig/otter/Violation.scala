package io.taig.otter

import cats.Eq
import cats.Show
import cats.derived.strict.*
import cats.parse.Parser
import cats.syntax.all.*

import java.util.regex.Pattern

final case class Violation(constraint: Constraint, actual: Data, hint: Option[String]) derives Eq:
  override def toString: String = Printers(this)

object Violation:
  def equal(reference: Data, actual: Data): Violation =
    Violation(Constraint.Equal(reference), actual, hint = none)

  def tpe(name: String, actual: Data): Violation = Violation(Constraint.Type(name), actual, hint = none)
  def tpe(name: String, actual: Data, hint: String): Violation =
    Violation(Constraint.Type(name), actual, hint = hint.some)

  def oneOf(values: List[Data.Primitive], actual: Data): Violation =
    Violation(Constraint.OneOf(values), actual, hint = none)

  def matches(pattern: Pattern, actual: Data): Violation =
    Violation(Constraint.Primitive.Matches(pattern), actual, hint = none)
  def matches(expected: String, actual: Data): Violation =
    matches(pattern = Pattern.compile(Pattern.quote(expected)), actual)

  def parse(value: String): Either[Parser.Error, Violation] = ??? // Parsers.violation.parseAll(value)

  given Show[Violation] = Show.fromToString
