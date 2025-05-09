package io.taig.otter.http

import cats.MonadThrow
import cats.syntax.all.*
import io.taig.otter.http.header.Accept
import org.http4s.Entity as Http4sBody
import org.http4s.Response as Http4sResponse
import org.http4s.Status as Http4sStatus

final class Http4sResultEncoder[F[_], S[_]](encoder: PayloadEncoder[S])(using F: MonadThrow[F]):
  def apply[A](result: Result[S, A], accept: Option[Accept], a: A): F[Option[Http4sResponse[F]]] = result match
    case Result.Modify(self, _, g)  => apply(result = self, accept, g(a))
    case Result.OrElse(left, right) => a.fold(apply(result = left, accept, _), apply(result = right, accept, _))
    case Result.Root(code, headers, bodies) =>
      F.fromEither(Http4sStatus.fromInt(code.toInt))
        .map: status =>
          bodies
            .fold(Http4sBody.empty.some)(Http4sBodiesEncoder(encoder)(_, accept, a._2))
            .map(Http4sResponse[F](status, headers = Http4sHeadersEncoder(codec = headers, a._1), _))
