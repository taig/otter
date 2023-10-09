package io.taig.otter.sample

import cats.data.Validated
import cats.effect.IO
import io.taig.otter.sample.api.Authentication
import io.taig.otter.validation.Violations
import munit.{Assertions, Location}

trait SampleAssertions:
  this: Assertions & SampleExtensions =>
  extension [O](self: IO[Validated[Violations, Either[Authentication.Error, O]]])
    def assertAuthenticated(using Location): IO[O] = self.toAuthenticated.attempt.map:
      case Right(o)    => o
      case Left(error) => fail(error.getMessage)

    def assertUnauthenticated(using Location): IO[Authentication.Error] = self.toUnauthenticated.attempt.map:
      case Right(e)    => e
      case Left(error) => fail(error.getMessage)

  extension [E <: Matchable, O](self: IO[Validated[Violations, Either[Authentication.Error, Either[E, O]]]])
    def assertSuccess(using Location): IO[O] = self.toSuccess.attempt.map:
      case Right(o)    => o
      case Left(error) => fail(error.getMessage)

    def assertError(using Location): IO[E] = self.toError.attempt.map:
      case Right(e)    => e
      case Left(error) => fail(error.getMessage)
