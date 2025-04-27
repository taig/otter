package io.taig.otter.sample

import cats.data.Validated
import cats.effect.IO
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.http.Client
import io.taig.otter.http.Route
import io.taig.otter.http.header.MediaType
import io.taig.otter.sample.api.AuthenticationApiSchema
import io.taig.otter.dsl.*
import io.taig.otter.sample.api.Role
import io.taig.otter.sample.api.RoleEndpoint
import io.taig.otter.sample.api.schema.SessionApiSchema

final class SampleClient(client: Client[IO]):
  def infallible[R <: Role, I, O](
      endpoint: RoleEndpoint[R, I, O],
      session: Option[SessionApiSchema],
      input: I,
      contentType: MediaType = mediaType.application.json
  ): IO[Validated[Violations, Either[Route.Error | AuthenticationApiSchema.Error, O]]] = client
    .submit(endpoint.toAuthenticatedEndpoint, contentType = contentType.some, AuthenticationApiSchema(session, input))
    .map(_.map {
      case Right(Right(o))    => o.asRight
      case Right(Left(error)) => error.asLeft
      case Left(error)        => error.asLeft
    })

  def fallible[R <: Role, E, I, O](
      endpoint: RoleEndpoint[R, I, Either[E, O]],
      session: Option[SessionApiSchema],
      input: I,
      contentType: MediaType = mediaType.application.json
  ): IO[Validated[Violations, Either[Route.Error | AuthenticationApiSchema.Error | E, O]]] =
    infallible(endpoint, session, input, contentType).map(_.map {
      case Right(Right(o))    => o.asRight
      case Right(Left(error)) => error.asLeft
      case Left(error)        => error.asLeft
    })
