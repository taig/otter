package io.taig.otter.validation

import io.taig.otter.Data

final case class Violation(constraint: Constraint, actual: Data)

object Violation:
  def tpe(name: String, actual: String): Violation = Violation(Constraint.Type(name), Data.String(actual))
  def tpe(name: String): Violation = Violation(Constraint.Type(name), actual = Data.Null)
  val required: Violation = Violation(Constraint.Required, actual = Data.Null)
