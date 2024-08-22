package io.taig.otter.sample

import cats.data.Validated
import cats.effect.IO
import io.taig.otter.sample.api.Role
import io.taig.otter.sample.api.AuthenticatedEndpoint
import io.taig.otter.sample.api.AuthenticationApiSchema
import io.taig.otter.Violations
import io.taig.otter.http.Client
import io.github.arainko.ducktape.*
import io.taig.otter.sample.app.transformers.given
import io.taig.otter.sample.api.schema.SessionApiSchema

final class SampleClient(client: Client[IO]):
  def submit[R <: Role, I, O](
      endpoint: AuthenticatedEndpoint[R, I, O],
      session: Option[Session],
      input: I
  ): IO[Validated[Violations, Either[AuthenticationApiSchema.Error, O]]] =
    client.submit(endpoint.toAuthenticatedEndpoint, AuthenticationApiSchema(session.map(_.to[SessionApiSchema]), input))

  def submit[R <: Role, I, O](
      endpoint: AuthenticatedEndpoint[R, I, O],
      input: I
  ): IO[Validated[Violations, Either[AuthenticationApiSchema.Error, O]]] = submit(endpoint, None, input)

  def submit[R <: Role, I, O](
      endpoint: AuthenticatedEndpoint[R, I, O],
      session: Session,
      input: I
  ): IO[Validated[Violations, Either[AuthenticationApiSchema.Error, O]]] =
    submit(endpoint, Some(session), input)
