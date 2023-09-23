package io.taig.otter.validation

import cats.syntax.all.*
import io.taig.otter.Schema
import io.taig.otter.schemas.*

final case class Violation(constraint: Constraint, actual: String)

object Violation:
  def tpe(name: String, actual: String): Violation = Violation(Constraint.Type(name), actual)
  def tpe(name: String): Violation = Violation(Constraint.Type(name), actual = "null")
  val required: Violation = Violation(Constraint.Required, actual = "null")
