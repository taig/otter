package io.taig.openapi.schema

import cats.data.Validated
import cats.syntax.all.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.openapi.validation.{Constraint, Validation}

private def typeViolations(tpe: String, actual: OpenApi): Violations =
  Violations.rootNec(Constraint.tpe(s"OpenApi.$tpe").map(OpenApi.fromString).toViolation(actual))

private[openapi] def applyValidation[A: Encoder, B, C](validation: Validation[A, B, B, C], encode: B => OpenApi)(
    b: B
): Validated[Violations, C] = validation
  .run(b)
  .leftMap: violations =>
    Violations.root(violations.map(_.mapReference(_.asOpenApi).mapActual(encode)))
