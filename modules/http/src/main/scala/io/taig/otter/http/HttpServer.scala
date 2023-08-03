package io.taig.otter.http

trait HttpServer[F[_], R]:
  def start(routes: Routes[F]): F[Unit]
