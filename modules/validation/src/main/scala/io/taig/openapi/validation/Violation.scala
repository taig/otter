package io.taig.openapi.validation

import cats.syntax.all.*

final case class Violation[+A](constraint: Constraint, actual: Option[A]):
  def map[B](f: A => B): Violation[B] = copy(actual = actual.map(f))

object Violation:
  def tpe[A](name: String, actual: A): Violation[A] = Violation(Constraint.Type(name), actual.some)
