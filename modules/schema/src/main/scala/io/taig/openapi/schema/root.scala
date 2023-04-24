package io.taig.openapi.schema

import cats.data.Validated
import cats.syntax.all.*
import io.taig.openapi.OpenApi
import io.taig.validation.{Constraint, Validation, Violation}

private def nonNullViolations(tpe: String): Violations =
  val constraint = Constraint("required", OpenApi.fromString(s"OpenApi.$tpe").some)
  Violations.rootNec(Violation(constraint, actual = OpenApi.Null))

private def typeViolations(tpe: String, actual: OpenApi): Violations =
  val constraint = Constraint("type", OpenApi.fromString(s"OpenApi.$tpe").some)
  Violations.rootNec(Violation(constraint, actual))

private[openapi] def andThenValidate[A, B](validation: Validation[A, A, A, B], encode: A => OpenApi)(
    a: A
): Validated[Violations, B] = validation
  .run(a)
  .leftMap: violations =>
    Violations.root(violations.map(_.mapReference(encode).mapActual(encode)))
