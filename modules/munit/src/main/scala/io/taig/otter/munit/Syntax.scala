package io.taig.otter.munit

import cats.data.Validated
import io.taig.otter.Violations
import cats.syntax.all.*
import cats.MonadThrow

trait Syntax:
  extension [F[_]: MonadThrow, O](self: F[Validated[Violations, O]])
    def toValid: F[O] = self.flatMap:
      case Validated.Valid(o) => o.pure
      case Validated.Invalid(violations) =>
        new IllegalArgumentException(s"Expected Valid, but got Violations: $violations").raiseError

    def toViolations: F[Violations] = self.flatMap:
      case Validated.Valid(o) =>
        new IllegalArgumentException(s"Expected Violations, but got Valid: $o").raiseError
      case Validated.Invalid(violations) => violations.pure

object Syntax extends Syntax
