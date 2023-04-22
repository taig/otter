package io.taig.openapi.sample

import cats.MonadThrow
import cats.data.Validated
import cats.effect.IO
import cats.syntax.all.*
import io.taig.openapi.*
import io.taig.openapi.http.{Client, OpenApiHttpException, Request, Response}
import io.taig.openapi.schema.Violations

import java.util.UUID

final class UnauthorizedClient(client: Client[IO]):
  def submit[I, O](endpoint: UnauthorizedEndpoint[I, O], input: I): IO[Validated[Violations, O]] =
    client.submit(endpoint, input)

  def submitOrError[I, O](endpoint: UnauthorizedEndpoint[I, O], input: I): IO[O] =
    submit(endpoint, input).flatMap(_.leftMap(OpenApiHttpException.apply).liftTo[IO])

  def submitOrFail[I, E <: Throwable, O](endpoint: UnauthorizedEndpoint[I, Either[E, O]], input: I): IO[O] =
    submitOrError(endpoint, input).flatMap {
      case Right(o)        => IO.pure(o)
      case Left(throwable) => IO.raiseError(throwable)
    }

object UnauthorizedClient:
  def apply(client: Client[IO]): UnauthorizedClient = new UnauthorizedClient(client)
