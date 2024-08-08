package io.taig.otter

import cats.parse.Parser
import cats.Show

final case class Violation(constraint: Constraint, actual: Data):
  override def toString: String = Printers(this)

object Violation:
  def parse(value: String): Either[Parser.Error, Violation] = Parsers.violation.parseAll(value)

  given Show[Violation] = Show.fromToString
