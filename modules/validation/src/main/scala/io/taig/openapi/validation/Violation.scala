package io.taig.openapi.validation

import cats.syntax.all.*
import io.taig.openapi.OpenApi

final case class Violation(constraint: Constraint, actual: Option[OpenApi])

object Violation:
  def apply(identifier: String, reference: Option[OpenApi.Primitive], actual: Option[OpenApi]): Violation =
    Violation(Constraint(identifier, reference), actual)

  def tpe(name: String, actual: OpenApi): Violation =
    Violation(identifier = "type", reference = OpenApi.fromString(name).some, actual.some)
