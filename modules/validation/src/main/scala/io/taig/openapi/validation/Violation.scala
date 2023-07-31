package io.taig.openapi.validation

import io.taig.openapi.OpenApi

final case class Violation(constraint: Constraint, actual: Option[OpenApi])

object Violation:
  def apply(identifier: String, actual: Option[OpenApi], reference: Option[OpenApi]): Violation =
    Violation(Constraint(identifier, actual), reference)
