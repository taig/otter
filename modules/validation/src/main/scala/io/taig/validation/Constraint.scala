package io.taig.validation

import io.taig.openapi.OpenApi

final case class Constraint(name: String, reference: Option[OpenApi], tpe: Constraint.Type)

object Constraint:
  enum Type:
    case Universal
    case Numeric(equal: Boolean, delta: Option[OpenApi])
