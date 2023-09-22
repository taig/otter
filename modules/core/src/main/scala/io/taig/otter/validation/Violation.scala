package io.taig.otter.validation

import cats.syntax.all.*
import io.taig.otter.Schema
import io.taig.otter.schemas.*

final case class Violation[A](constraint: Constraint, actual: A, schema: Schema[A])

object Violation:
  def tpe(name: String, actual: String): Violation[String] = Violation(Constraint.Type(name), actual, string)
  def tpe(name: String): Violation[String] = Violation(Constraint.Type(name), actual = "null", string)
  val required: Violation[String] = Violation(Constraint.Required, actual = "null", string)
  def required[A](actual: A, schema: Schema[A]): Violation[A] = Violation(Constraint.Required, actual, schema)
