package io.taig.openapi.schema

import cats.data.Validated
import cats.syntax.all.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.syntax.*
import io.taig.openapi.validation.{Constraint, Validation, Violation}

private def nonNullViolations(tpe: String): Violations =
  val constraint = Constraint("required", OpenApi.fromString(s"OpenApi.$tpe").some)
  Violations.rootNec(Violation(constraint, actual = OpenApi.Null))

private def typeViolations(tpe: String, actual: OpenApi): Violations =
  val constraint = Constraint("type", OpenApi.fromString(s"OpenApi.$tpe").some)
  Violations.rootNec(Violation(constraint, actual))

private[openapi] def andThenValidate[A: Encoder, B, C](validation: Validation[A, B, B, C], encode: B => OpenApi)(
    b: B
): Validated[Violations, C] = validation
  .run(b)
  .leftMap: violations =>
    Violations.root(violations.map(_.mapReference(_.asOpenApi).mapActual(encode)))
