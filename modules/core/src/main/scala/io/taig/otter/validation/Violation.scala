package io.taig.otter.validation

import io.taig.otter.Data

final case class Violation(constraint: Constraint, actual: Data)

object Violation:
  def tpe(name: String, actual: Data): Violation = Violation(Constraint.Type(name), actual)
  def tpe(name: String, actual: String): Violation = tpe(name, Data.String(actual))
  def tpe(name: String): Violation = tpe(name, actual = Data.Null)
  val required: Violation = Violation(Constraint.Required, actual = Data.Null)
