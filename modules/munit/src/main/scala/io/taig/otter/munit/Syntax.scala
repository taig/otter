package io.taig.otter.munit

import cats.data.Validated
import io.taig.otter.Violations
import cats.syntax.all.*
import cats.MonadThrow

trait Syntax:
  extension [F[_]: MonadThrow, E, O](self: F[Validated[Violations, Either[E, O]]])
    def toValid: F[Either[E, O]] = self.flatMap:
      case Validated.Valid(o) => o.pure
      case Validated.Invalid(violations) =>
        new IllegalArgumentException(s"Expected Valid, but got Violations: $violations").raiseError

    def toSuccess: F[O] = toValid.flatMap:
      case Right(o)    => o.pure
      case Left(error) => new IllegalArgumentException(s"Expected Success, but got Error: $error").raiseError

    def toError: F[E] = toValid.flatMap:
      case Right(o)    => new IllegalArgumentException(s"Expected Error, but got Success: $o").raiseError
      case Left(error) => error.pure

    def toViolations: F[Violations] = self.flatMap:
      case Validated.Valid(o) =>
        new IllegalArgumentException(s"Expected Violations, but got Valid: $o").raiseError
      case Validated.Invalid(violations) => violations.pure

object Syntax extends Syntax
