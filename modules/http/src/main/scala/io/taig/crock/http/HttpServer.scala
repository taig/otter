package io.taig.crock.http

trait HttpServer[F[_], R]:
  def start(routes: Routes[F]): F[Unit]
