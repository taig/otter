package io.taig.validation

import io.taig.openapi.OpenApi

final case class Violation(constraint: Constraint, actual: OpenApi)
