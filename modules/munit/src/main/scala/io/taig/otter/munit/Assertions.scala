package io.taig.otter.munit

import cats.data.Validated
import cats.effect.IO
import munit.{Assertions as MunitAssertions, Location}
import io.taig.otter.Violations

trait Assertions extends MunitAssertions, Syntax:
  extension [E, O](self: IO[Validated[Violations, Either[E, O]]])
    def assertValid(using Location): IO[Either[E, O]] = self.toValid
    def assertSuccess(using Location): IO[O] = self.toSuccess
    def assertError(using Location): IO[E] = self.toError
    def assertViolations(using Location): IO[Violations] = self.toViolations
