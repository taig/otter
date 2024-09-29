package io.taig.otter.server

import cats.effect.Resource
import io.taig.otter.http.App

trait Server[F[_]]:
  def start(app: App[F]): Resource[F, String]
