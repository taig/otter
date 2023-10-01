package io.taig.otter.sample

import cats.data.Validated
import cats.syntax.all.*
import cats.effect.IO
import io.taig.otter.http.{Client, ViolationsException}
import io.taig.otter.sample.api.endpoints.{Authentication, Endpoint}
import io.taig.otter.validation.Violations

import java.util.UUID

final class SampleClient(client: Client[IO]) extends Client[IO]:
  def submit[R, I, O](
      endpoint: Endpoint[R, I, O],
      session: Option[UUID],
      input: I
  ): IO[Validated[Violations, Either[Authentication.Error, O]]] =
    client.submit(endpoint, Authentication(???, session, input))

  def submitValid[R, I, O](
      endpoint: Endpoint[R, I, O],
      session: Option[UUID],
      input: I
  ): IO[Either[Authentication.Error, O]] = submit(endpoint, session, input).flatMap(_.leftMap { violations =>
    val cause = ViolationsException(violations)
    new IllegalStateException("Expected valid, but got invalid", cause)
  }.liftTo[IO])

  def submitInvalid[R, I, O](
      endpoint: Endpoint[R, I, O],
      session: Option[UUID],
      input: I
  ): IO[Violations] = submit(endpoint, session, input)
    .flatMap(_.fold(IO.pure, _ => IO.raiseError(new IllegalStateException("Expected invalid, but got valid"))))

  def submitAuthenticated[R, I, O](
      endpoint: Endpoint[R, I, O],
      session: Option[UUID],
      input: I
  ): IO[O] = submitValid(endpoint, session, input)
    .map(_.leftMap(error => new IllegalStateException("Expected authenticated, but got error", error)))
    .rethrow

  def submitSuccess[R, I, E <: Matchable, A](
      endpoint: Endpoint[R, I, Either[E, A]],
      session: Option[UUID],
      input: I
  ): IO[A] = submitAuthenticated(endpoint, session, input)
    .map(_.leftMap {
      case error: Throwable => new IllegalStateException("Expected success, but got error", error)
      case error            => new IllegalStateException(s"Expected success, but got error ($error)")
    })
    .rethrow

  def submitError[R, I, E, A](
      endpoint: Endpoint[R, I, Either[E, A]],
      session: Option[UUID],
      input: I
  ): IO[E] = submitAuthenticated(endpoint, session, input)
    .map(_.swap.leftMap(a => new IllegalStateException(s"Expected error, but got success (${a})")))
    .rethrow
