package io.taig.otter.munit

import cats.MonadThrow
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import munit.Assertions as MunitAssertions

trait Assertions extends MunitAssertions:
  extension [F[_]: MonadThrow, E, O](self: F[Validated[Violations, Either[E, O]]])
    def assertValid: F[Either[E, O]] = self.flatMap:
      case Validated.Valid(o)            => o.pure
      case Validated.Invalid(violations) =>
        new IllegalArgumentException(s"Expected Valid, but got Violations: $violations").raiseError

    def assertSuccess: F[O] = assertValid.flatMap:
      case Right(o)    => o.pure
      case Left(error) => new IllegalArgumentException(s"Expected Success, but got Error: $error").raiseError

    def assertError: F[E] = assertValid.flatMap:
      case Right(o)    => new IllegalArgumentException(s"Expected Error, but got Success: $o").raiseError
      case Left(error) => error.pure

    def assertViolations: F[Violations] = self.flatMap:
      case Validated.Valid(o) =>
        new IllegalArgumentException(s"Expected Violations, but got Valid: $o").raiseError
      case Validated.Invalid(violations) => violations.pure

object Assertions extends Assertions
