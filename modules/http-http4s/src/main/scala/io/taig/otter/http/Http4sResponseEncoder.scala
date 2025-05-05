package io.taig.otter.http

import org.http4s.Response as Http4sResponse
import cats.data.Validated

final class Http4sResponseEncoder[F[_], S[_], T[_]]:
  def apply[A](response: Response[S, T, A], a: Validated[Response.Error, A]): F[Http4sResponse[F]] = ???