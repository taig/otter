package io.taig.otter.validation

import cats.syntax.all.*
import io.taig.otter.OpenApi

final case class Violation(constraint: Constraint, actual: Option[OpenApi])

object Violation:
  def tpe(name: String, actual: String): Violation = Violation(Constraint.Type(name), OpenApi.Text(actual).some)
  val required: Violation = Violation(Constraint.Required, actual = none)
  def required(actual: OpenApi): Violation = Violation(Constraint.Required, actual.some)
