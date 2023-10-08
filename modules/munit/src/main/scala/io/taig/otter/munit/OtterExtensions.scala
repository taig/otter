package io.taig.otter.munit

import cats.MonadThrow
import cats.syntax.all.*
import cats.data.Validated
import io.taig.otter.http.ViolationsException
import io.taig.otter.validation.Violations

trait OtterExtensions:
  extension [F[_]: MonadThrow, O](self: F[Validated[Violations, O]])
    def toValid: F[O] = self.flatMap(_.leftMap { violations =>
      new IllegalStateException("Expected Valid, got Invalid", ViolationsException(violations))
    }.liftTo[F])

    def toViolations: F[Violations] = self
      .flatMap(_.swap.leftMap(_ => new IllegalStateException("Expected Invalid, got Valid")).liftTo[F])
