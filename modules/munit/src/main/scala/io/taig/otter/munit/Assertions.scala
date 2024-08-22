package io.taig.otter.munit

import cats.data.Validated
import cats.effect.IO
import munit.{Assertions as MunitAssertions, Location}
import io.taig.otter.Violations

trait Assertions:
  this: MunitAssertions =>

  extension [O](self: IO[Validated[Violations, O]])
    def assertValid(using Location): IO[O] = self.attempt.map:
      case Right(Validated.Valid(o))            => o
      case Right(Validated.Invalid(violations)) => fail(s"Expected Valid, but got Violations:\n$violations")
      case Left(error)                          => fail(error.getMessage)

    def assertViolations(using Location): IO[Violations] = self.attempt.map:
      case Right(Validated.Valid(o))            => fail(s"Expected Violations, but got Valid:\n$o")
      case Right(Validated.Invalid(violations)) => violations
      case Left(error)                          => fail(error.getMessage)
