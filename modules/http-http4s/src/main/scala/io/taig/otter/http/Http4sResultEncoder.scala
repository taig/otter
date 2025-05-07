package io.taig.otter.http

import org.http4s.Status as Http4sStatus
import org.http4s.Response as Http4sResponse
import cats.MonadThrow
import cats.syntax.all.*
import io.taig.otter.http.header.Accept

final class Http4sResultEncoder[F[_], S[_]](encoder: PayloadEncoder[S])(using F: MonadThrow[F]):
  def apply[A](result: Result[S, A], accept: Option[Accept], a: A): F[Http4sResponse[F]] = result match
    case Result.Modify(self, _, g)  => apply(result = self, accept, g(a))
    case Result.OrElse(left, right) => a.fold(apply(result = left, accept, _), apply(result = right, accept, _))
    case Result.Root(code, headers, body) =>
      ???
      // F.fromEither(Http4sStatus.fromInt(code.toInt))
      //   .map: status =>
      //     Http4sResponse[F](
      //       status,
      //       headers = Http4sHeadersEncoder(codec = headers, a._1),
      //       entity = Http4sBodyEncoder(encoder)(body, accept, a._2)
      //     )
