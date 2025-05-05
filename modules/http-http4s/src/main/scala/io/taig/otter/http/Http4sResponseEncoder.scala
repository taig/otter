package io.taig.otter.http

import org.http4s.Response as Http4sResponse
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.MonadThrow
import cats.syntax.all.*

final class Http4sResponseEncoder[F[_]: MonadThrow, S[_], T[_]]:
  def apply[A](
      response: Response[S, T, A],
      result: Either[Throwable, Validated[Response.Error, A]]
  ): F[Http4sResponse[F]] =
    result match
      case Left(throwable)                 =>
        // TODO print stacktrace if desired
        Http4sResultEncoder[F, T](encoder = ???).apply(result = response.failure, none)
      case Right(Validated.Invalid(error)) =>
        Http4sResultEncoder[F, T](encoder = ???).apply(result = response.error, error)
      case Right(Validated.Valid(a)) =>
        Http4sResultEncoder[F, S](encoder = ???).apply(result = response.result, a)
