package io.taig.otter

import cats.Eq
import cats.Show
import cats.derived.strict.*
import cats.parse.Parser

final case class Violation(constraint: Constraint, actual: Data.Any) derives Eq:
  override def toString: String = ??? // Printers(this)

object Violation:
  def tpe(name: String, actual: Data.Any): Violation = Violation(Constraint.Type(name), actual)

  def oneOf(values: List[Data.Primitive], actual: Data.Any): Violation = Violation(Constraint.OneOf(values), actual)

  def parse(value: String): Either[Parser.Error, Violation] = ??? // Parsers.violation.parseAll(value)

  given Show[Violation] = Show.fromToString
