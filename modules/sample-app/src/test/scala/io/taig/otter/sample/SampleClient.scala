package io.taig.otter.sample

import cats.data.Validated
import cats.effect.IO
import cats.syntax.all.*
import io.taig.otter.sample.api.Role
import io.taig.otter.sample.api.RoleEndpoint
import io.taig.otter.sample.api.AuthenticationApiSchema
import io.taig.otter.Violations
import io.taig.otter.http.Client
import io.github.arainko.ducktape.*
import io.taig.otter.sample.app.transformers.given
import io.taig.otter.sample.api.schema.SessionApiSchema
import io.taig.otter.http.Route

final class SampleClient(client: Client[IO]):
  def submit[R <: Role, I, O](
      endpoint: RoleEndpoint[R, I, O],
      session: Option[Session],
      input: I
  ): IO[Validated[Violations, Either[Route.Error | AuthenticationApiSchema.Error, O]]] = client
    .submit(endpoint.toAuthenticatedEndpoint, AuthenticationApiSchema(session.map(_.to[SessionApiSchema]), input))
    .map(_.map {
      case Right(Right(o))    => o.asRight
      case Right(Left(error)) => error.asLeft
      case Left(error)        => error.asLeft
    })

  inline def submit[R <: Role, E, I, O](
      endpoint: RoleEndpoint[R, I, Either[E, O]],
      session: Option[Session],
      input: I
  ): IO[Validated[Violations, Either[Route.Error | AuthenticationApiSchema.Error | E, O]]] =
    submit[R, I, Either[E, O]](endpoint, session, input).map(_.map {
      case Right(Right(o))    => o.asRight
      case Right(Left(error)) => error.asLeft
      case Left(error)        => error.asLeft
    })

  def submit[R <: Role, I, O](
      endpoint: RoleEndpoint[R, I, O],
      input: I
  ): IO[Validated[Violations, Either[Route.Error | AuthenticationApiSchema.Error, O]]] = submit(endpoint, None, input)

  inline def submit[R <: Role, E, I, O](
      endpoint: RoleEndpoint[R, I, Either[E, O]],
      input: I
  ): IO[Validated[Violations, Either[Route.Error | AuthenticationApiSchema.Error | E, O]]] =
    submit(endpoint, None, input)

  def submit[R <: Role, I, O](
      endpoint: RoleEndpoint[R, I, O],
      session: Session,
      input: I
  ): IO[Validated[Violations, Either[Route.Error | AuthenticationApiSchema.Error, O]]] =
    submit(endpoint, Some(session), input)
