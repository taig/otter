package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import cats.{Monad, MonadThrow}
import io.taig.otter.validation.Violations

trait Client[F[_]]:
  def submit(request: Http.Request): F[Http.Response]

  final def submit[I, O](endpoint: Endpoint[I, O], input: I)(using Monad[F]): F[Validated[Violations, O]] =
    submit(endpoint.request.encode(input)).map(endpoint.response.decode)

  final def submitValid[I, O](
      endpoint: Endpoint[I, O],
      input: I
  )(using MonadThrow[F]): F[O] = submit(endpoint, input).flatMap(_.leftMap { violations =>
    val cause = ViolationsException(violations)
    new IllegalStateException("Expected valid, but got invalid", cause)
  }.liftTo[F])

  final def submitInvalid[I, O](
      endpoint: Endpoint[I, O],
      input: I
  )(using MonadThrow[F]): F[Violations] = submit(endpoint, input)
    .flatMap(_.fold(_.pure, _ => new IllegalStateException("Expected invalid, but got valid").raiseError))
