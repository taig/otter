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

private[openapi] def andThenValidate[A, B, C, D](validation: Validation[A, B, C, D], encode: B => OpenApi)(
    b: B
): Validated[Violations, D] = ???
//validation.run(b).leftMap { violations =>
//  Violations.root(violations.map(_.mapConstraint(_.asOpenApi).mapActual(encode)))
//}
