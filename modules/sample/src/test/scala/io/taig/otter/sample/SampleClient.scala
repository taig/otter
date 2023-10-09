package io.taig.otter.sample

import cats.data.Validated
import cats.effect.IO
import io.taig.otter.http.Client
import io.taig.otter.sample.api.{Authentication, Role}
import io.taig.otter.sample.api.endpoints.AuthenticatedEndpoint
import io.taig.otter.sample.data.Session
import io.taig.otter.validation.Violations

final class SampleClient(client: Client[IO]):
  def submit[R <: Role, I, O](
      endpoint: AuthenticatedEndpoint[R, I, O],
      session: Option[Session],
      input: I
  ): IO[Validated[Violations, Either[Authentication.Error, O]]] =
    client.submit(endpoint.toAuthenticatedEndpoint, Authentication(session, input))

  def submit[R <: Role, I, O](
      endpoint: AuthenticatedEndpoint[R, I, O],
      input: I
  ): IO[Validated[Violations, Either[Authentication.Error, O]]] =
    submit(endpoint, None, input)

  def submit[R <: Role, I, O](
      endpoint: AuthenticatedEndpoint[R, I, O],
      session: Session,
      input: I
  ): IO[Validated[Violations, Either[Authentication.Error, O]]] =
    submit(endpoint, Some(session), input)
