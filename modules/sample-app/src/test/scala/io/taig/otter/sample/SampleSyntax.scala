package io.taig.otter.sample

import cats.MonadThrow
import cats.syntax.all.*
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.sample.api.AuthenticationApiSchema
import io.taig.otter.munit.Syntax
import io.taig.otter.http.Route

trait SampleSyntax extends Syntax:
  extension [F[_]: MonadThrow, O](
      self: F[Validated[Violations, Either[Route.Error | AuthenticationApiSchema.Error, O]]]
  )
    def toAuthenticated: F[O] = self.toValid.flatMap:
      case Right(o) => o.pure
      case Left(error) =>
        new IllegalArgumentException(s"Expected Authenticated, but got Error: $error").raiseError

    def toUnauthenticated: F[AuthenticationApiSchema.Error] = self.toValid.flatMap:
      case Right(o) =>
        new IllegalArgumentException(s"Expected Error, but got Authenticated: $o").raiseError
      case Left(error) => ??? // error.pure
