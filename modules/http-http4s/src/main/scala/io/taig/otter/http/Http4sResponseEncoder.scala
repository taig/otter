package io.taig.otter.http

import io.taig.otter.+
import org.http4s.Response as Http4sResponse
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.MonadThrow
import io.taig.otter.http.header.Accept

final class Http4sResponseEncoder[F[_]: MonadThrow, S[_], T[_]](encoder: PayloadEncoder[S + T], debug: Boolean):
  val results = Http4sResultEncoder(encoder)

  def apply[A](
      response: Response[S, T, A],
      accept: Option[Accept],
      result: Either[Throwable, Validated[Response.Error, A]]
  ): F[Http4sResponse[F]] = result match
    case Left(throwable) =>
      results(result = response.failure, accept, Option.when(debug)(StacktracePrinter(throwable)))
    case Right(Validated.Invalid(error)) => results(result = response.error, accept, error)
    case Right(Validated.Valid(a))       => results(result = response.result, accept, a)
