package io.taig.openapi.validation

import cats.syntax.all.*

final case class Violation(constraint: Constraint, actual: Option[String])

object Violation:
  def tpe(name: String, actual: String): Violation = Violation(Constraint.Type(name), actual.some)
