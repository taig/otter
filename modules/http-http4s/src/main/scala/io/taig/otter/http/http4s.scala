package io.taig.otter.http

import cats.Monad
import io.taig.otter.http.header.MediaType
import org.http4s.HttpRoutes as Http4sRoutes

def toHttp4sRoutes[F[_]: Monad, S[_], T[_], U[_]](
    routes: Routes[F, S, T, U],
    encode: [A] => (MediaType, T[A]) => String
): Http4sRoutes[F] =
  Http4sRoutes: request =>
    // Http4sHeaderDecoder()
    ???
