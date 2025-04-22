package io.taig.otter.http

import org.http4s.HttpRoutes as Http4sRoutes
import io.taig.otter.http.header.MediaType
import cats.Monad

def toHttp4sRoutes[F[_]: Monad, S, T](routes: Routes[F, S, T], encode: (MediaType, T) => String): Http4sRoutes[F] =
  Http4sRoutes: request =>
    // Http4sHeaderDecoder()
    ???
