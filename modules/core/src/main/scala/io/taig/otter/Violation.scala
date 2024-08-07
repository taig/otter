package io.taig.otter

import cats.parse.Parser

final case class Violation(constraint: Constraint, actual: Data):
  def print: String = Printers(this)
  override def toString: String = print

object Violation:
  def parse(value: String): Either[Parser.Error, Violation] = Parsers.violation.parseAll(value)
