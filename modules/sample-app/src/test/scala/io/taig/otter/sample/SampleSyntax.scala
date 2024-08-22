package io.taig.otter.sample

import cats.MonadThrow
import cats.syntax.all.*
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.sample.api.AuthenticationApiSchema
import io.taig.otter.munit.Syntax

trait SampleSyntax extends Syntax:
  extension [F[_]: MonadThrow, O](self: F[Validated[Violations, Either[AuthenticationApiSchema.Error, O]]])
    def toAuthenticated: F[O] = self.toValid.flatMap:
      case Right(o) => o.pure
      case Left(error) =>
        new IllegalArgumentException(s"Expected Authenticated, but got Error: $error").raiseError

    def toUnauthenticated: F[AuthenticationApiSchema.Error] = self.toValid.flatMap:
      case Right(o) =>
        new IllegalArgumentException(s"Expected Error, but got Authenticated: $o").raiseError
      case Left(error) => error.pure

  extension [F[_]: MonadThrow, E <: Matchable, O](
      self: F[Validated[Violations, Either[AuthenticationApiSchema.Error, Either[E, O]]]]
  )
    def toSuccess: F[O] = self.toAuthenticated.flatMap:
      case Right(o) => o.pure
      case Left(error: Throwable) =>
        new IllegalArgumentException(s"Expected Success, but got Error", error).raiseError
      case Left(error) =>
        new IllegalArgumentException(s"Expected Success, but got Error: $error").raiseError

    def toError: F[E] = self.toAuthenticated.flatMap:
      case Left(error) => error.pure
      case Right(o) =>
        new IllegalArgumentException(s"Expected Error, but got Authenticated: $o").raiseError
