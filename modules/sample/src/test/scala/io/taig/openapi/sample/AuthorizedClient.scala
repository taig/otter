package io.taig.openapi.sample

import cats.MonadThrow
import cats.data.Validated
import cats.effect.IO
import cats.syntax.all.*
import io.taig.openapi.*
import io.taig.openapi.http.{Client, OpenApiHttpException}
import io.taig.openapi.schema.Violations

import java.util.UUID

final class AuthorizedClient(client: Client[IO]):
  def submit[I, O](
      endpoint: AuthorizedEndpoint[I, O],
      token: UUID,
      input: I
  ): IO[Validated[Violations, Authorization.Error | O]] = client.submit(endpoint.endpoint, Authorization(token, input))

  def submitOrAuthorization[I, O](
      endpoint: AuthorizedEndpoint[I, O],
      token: UUID,
      input: I
  ): IO[Authorization.Error | O] =
    submit(endpoint, token, input).flatMap(_.leftMap(new OpenApiHttpException(_)).liftTo[IO])

  inline def submitOrError[I, O <: Matchable](endpoint: AuthorizedEndpoint[I, O], token: UUID, input: I): IO[O] =
    submit(endpoint, token, input).flatMap {
      case Validated.Valid(o: O)                       => IO.pure(o)
      case Validated.Valid(error: Authorization.Error) => IO.raiseError(error)
      case Validated.Invalid(violations)               => IO.raiseError(new OpenApiHttpException(violations))
    }

  def submitOrFail[I, E <: Throwable, O](endpoint: AuthorizedEndpoint[I, Either[E, O]], token: UUID, input: I): IO[O] =
    submitOrError(endpoint, token, input).flatMap {
      case Right(o)        => IO.pure(o)
      case Left(throwable) => IO.raiseError(throwable)
    }

object AuthorizedClient:
  def apply(client: Client[IO]): AuthorizedClient = new AuthorizedClient(client)
