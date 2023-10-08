package io.taig.otter.munit

import cats.data.Validated
import cats.effect.IO
import io.taig.otter.validation.Violations
import munit.{Assertions, Location}

trait OtterAssertions:
  this: Assertions & OtterExtensions =>
  extension [O](self: IO[Validated[Violations, O]])
    def assertValid(using Location): IO[O] = self.toValid.attempt.map:
      case Right(o)    => o
      case Left(error) => fail(error.getMessage)

    def assertViolations(using Location): IO[Violations] = self.toViolations.attempt.map:
      case Right(violations) => violations
      case Left(error)       => fail(error.getMessage)
