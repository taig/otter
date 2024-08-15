package io.taig.otter.server

import io.taig.otter.http.App
import cats.effect.Resource

trait Server[F[_]]:
  def start(app: App[F], onError: Throwable => F[Unit]): Resource[F, String]

  // def apply(request: Http.Request): F[Http.Response]
