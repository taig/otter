package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import cats.{Monad, MonadThrow}
import io.taig.otter.validation.Violations

trait Client[F[_]]:
  def submit(request: Http.Request): F[Http.Response]

  final def submit[I, O](endpoint: Endpoint[I, O], input: I)(using Monad[F]): F[Validated[Violations, O]] =
    submit(endpoint.request.encode(input)).map(endpoint.response.decode)

  final def submitOrFail[I, O](endpoint: Endpoint[I, O], input: I)(using MonadThrow[F]): F[O] =
    submit(endpoint, input).flatMap(_.leftMap(ViolationsException(_)).liftTo[F])
