package io.taig.otter.sample

import cats.data.Validated
import cats.effect.IO
import io.taig.otter.munit.Assertions
import io.taig.otter.Violations
import io.taig.otter.sample.api.AuthenticationApiSchema
import munit.Location

trait SampleAssertions extends Assertions, SampleSyntax:
  extension [O](self: IO[Validated[Violations, Either[AuthenticationApiSchema.Error, O]]])
    def assertAuthenticated(using Location): IO[O] = self.toAuthenticated
    def assertUnauthenticated(using Location): IO[AuthenticationApiSchema.Error] = self.toUnauthenticated

  extension [E <: Matchable, O](self: IO[Validated[Violations, Either[AuthenticationApiSchema.Error, Either[E, O]]]])
    def assertSuccess(using Location): IO[O] = self.toSuccess
    def assertError(using Location): IO[E] = self.toError
