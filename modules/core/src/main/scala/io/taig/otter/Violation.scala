package io.taig.otter

import cats.Show
import cats.parse.Parser

final case class Violation(constraint: Constraint, actual: Data):
  override def toString: String = Printers(this)

object Violation:
  def tpe(name: String, actual: Data): Violation = Violation(Constraint.Type(name), actual)
  def tpe(name: String, actual: String): Violation = Violation(Constraint.Type(name), Data.String(actual))

  def oneOf(values: List[Data.Primitive], actual: Data): Violation = Violation(Constraint.OneOf(values), actual)
  def oneOf(values: List[String], actual: String): Violation =
    oneOf(values.map(Data.String.apply), Data.String(actual))

  def parse(value: String): Either[Parser.Error, Violation] = Parsers.violation.parseAll(value)

  given Show[Violation] = Show.fromToString
