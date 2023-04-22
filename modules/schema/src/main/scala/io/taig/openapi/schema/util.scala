package io.taig.openapi.schema

import cats.Applicative
import cats.data.{Chain, Validated}
import cats.syntax.all.*
import io.taig.openapi.syntax.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.validation.syntax.*
import io.taig.validation.*

import scala.deriving.Mirror
import scala.quoted.{Type as QType, *}

private def refine[A](tpe: String)(f: OpenApi => Option[A]): Validation[OpenApi, OpenApi, OpenApi, A] =
  validations.refine(tpe)(f).mapReference(OpenApi.fromString)

private def nonNullViolations(tpe: String): Violations =
  val constraint = identifiers.required.toConstraint(reference = OpenApi.fromString(s"OpenApi.$tpe").some)
  Violations.rootNec(Violation(constraint, actual = OpenApi.Null))

private def typeViolations(tpe: String, actual: OpenApi): Violations =
  val constraint = identifiers.tpe.toConstraint(reference = OpenApi.fromString(s"OpenApi.$tpe").some)
  Violations.rootNec(Violation(constraint, actual))

private[openapi] def andThenValidate[A: Encoder, B, C](validation: Validation[A, B, B, C], encode: B => OpenApi)(
    b: B
): Validated[Violations, C] = validation.run(b).leftMap { violations =>
  Violations.root(violations.map(_.mapConstraint(_.asOpenApi).mapActual(encode)))
}

private[openapi] def andThenValidateF[F[+_]: Applicative, A: Encoder, B, C](
    validation: Validation[A, B, B, C],
    encode: B => F[OpenApi]
)(
    b: B
): F[Validated[Violations, C]] = validation.run(b) match
  case Validated.Valid(c) => c.valid.pure[F]
  case Validated.Invalid(violations) =>
    violations.traverse(_.mapConstraint(_.asOpenApi).traverseActual(b => encode(b))).map(Violations.root(_).invalid)
