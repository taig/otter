package io.taig.otter.server

import io.taig.otter.http.App
import cats.effect.Resource

trait Server[F[_]]:
  def start(app: App[F]): Resource[F, String]
