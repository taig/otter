package io.taig.otter.munit

import cats.data.Validated
import cats.effect.IO
import munit.{Assertions as MunitAssertions, Location}
import io.taig.otter.Violations

trait Assertions extends MunitAssertions, Syntax:
  extension [O](self: IO[Validated[Violations, O]])
    def assertValid(using Location): IO[O] = self.toValid
    def assertViolations(using Location): IO[Violations] = self.toViolations
