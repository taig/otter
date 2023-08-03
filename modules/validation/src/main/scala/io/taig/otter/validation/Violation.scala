package io.taig.otter.validation

import cats.syntax.all.*

final case class Violation(constraint: Constraint, actual: Option[String])

object Violation:
  def tpe(name: String, actual: String): Violation = Violation(Constraint.Type(name), actual.some)
  val required: Violation = Violation(Constraint.Required, actual = none)
  def required(actual: String): Violation = Violation(Constraint.Required, actual.some)
