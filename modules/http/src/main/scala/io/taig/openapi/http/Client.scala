package io.taig.openapi.http

import cats.data.Validated
import cats.syntax.all.*
import cats.{Applicative, ApplicativeThrow, Functor, Monad, MonadThrow}
import io.taig.openapi.schema.Violations

abstract class Client[F[+_]: ApplicativeThrow]:
  def submitRaw[I, O](endpoint: Endpoint[I, O], request: Request[F]): F[Response]

  final def submit[I, O](endpoint: Endpoint[I, O], input: I)(using Monad[F]): F[Validated[Violations, O]] =
    endpoint.input.encode(input).flatMap(submitRaw(endpoint, _)).map(endpoint.output.decode)

  final def submitOrFail[I, O](endpoint: Endpoint[I, O], input: I)(using MonadThrow[F]): F[O] =
    submit(endpoint, input).flatMap(_.leftMap(OpenApiHttpException.apply).liftTo[F])
