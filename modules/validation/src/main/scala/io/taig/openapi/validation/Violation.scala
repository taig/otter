package io.taig.openapi.validation

import cats.syntax.all.*
import io.taig.openapi.OpenApi

final case class Violation(constraint: Constraint, actual: Option[OpenApi])

object Violation:
  def tpe(name: String, actual: OpenApi): Violation = Violation(Constraint.Type(name), actual.some)
