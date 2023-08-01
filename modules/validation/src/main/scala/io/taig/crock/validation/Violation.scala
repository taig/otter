package io.taig.crock.validation

import cats.syntax.all.*

final case class Violation(constraint: Constraint, actual: Option[String])

object Violation:
  def tpe(name: String, actual: String): Violation = Violation(Constraint.Type(name), actual.some)
  val required: Violation = Violation(Constraint.Required, actual = none)
