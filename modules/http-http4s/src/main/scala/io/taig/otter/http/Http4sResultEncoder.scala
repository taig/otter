package io.taig.otter.http

import org.http4s.Status as Http4sStatus
import org.http4s.Response as Http4sResponse
import cats.MonadThrow
import cats.syntax.all.*
import java.nio.charset.StandardCharsets

final class Http4sResultEncoder[F[_], S[_]](encoder: BodyEncoder[S])(using F: MonadThrow[F]):
  def apply[A](result: Result[S, A], a: A): F[Http4sResponse[F]] = result match
    case Result.Modify(self, _, g)  => apply(result = self, g(a))
    case Result.OrElse(left, right) => a.fold(apply(result = left, _), apply(result = right, _))
    case Result.Root(code, headers, body) =>
      F.fromEither(Http4sStatus.fromInt(code.toInt))
        .map: status =>
          Http4sResponse[F](
            status,
            headers = Http4sHeadersEncoder(codec = headers, a._1),
            // TODO where to get the actual charset?
            entity = Http4sBodyEncoder(encoder).apply(charset = StandardCharsets.UTF_8.some, body, a._2)
          )
