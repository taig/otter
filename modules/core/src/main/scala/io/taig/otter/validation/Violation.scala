package io.taig.otter.validation

import cats.syntax.all.*
import io.taig.otter.OpenApi

final case class Violation(constraint: Constraint, actual: OpenApi)

object Violation:
  def tpe(name: String, actual: String): Violation = Violation(Constraint.Type(name), OpenApi.String(actual))
  def tpe(name: String): Violation = Violation(Constraint.Type(name), OpenApi.Null)
  val required: Violation = Violation(Constraint.Required, actual = OpenApi.Null)
  def required(actual: OpenApi): Violation = Violation(Constraint.Required, actual)
