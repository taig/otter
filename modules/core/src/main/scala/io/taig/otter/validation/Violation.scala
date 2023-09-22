package io.taig.otter.validation

import cats.syntax.all.*

final case class Violation[+A](constraint: Constraint, actual: Option[A])

object Violation:
  def tpe(name: String, actual: String): Violation[String] = Violation(Constraint.Type(name), actual.some)
  def tpe(name: String): Violation[Nothing] = Violation(Constraint.Type(name), actual = none)
  val required: Violation[Nothing] = Violation(Constraint.Required, actual = none)
  def required[A](actual: A): Violation[A] = Violation(Constraint.Required, actual.some)
