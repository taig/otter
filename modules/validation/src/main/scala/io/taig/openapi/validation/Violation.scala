package io.taig.openapi.validation

import cats.syntax.all.*

final case class Violation[+A](constraint: Constraint, actual: Option[A])

object Violation:
  def tpe[A](name: String, actual: A): Violation[A] = Violation(Constraint.Type(name), actual.some)
