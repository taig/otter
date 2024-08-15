package io.taig.otter.server

import cats.effect.Resource
import org.http4s.HttpApp as Http4sApp
import org.http4s.server.Server as Underlying
import io.taig.otter.http.*
import cats.syntax.all.*
import io.taig.otter.http.http4s.*
import cats.effect.Concurrent

final class Http4sServer[F[_]: Concurrent](f: Http4sApp[F] => Resource[F, Underlying]) extends Server[F]:
  override def start(app: App[F], onError: Throwable => F[Unit]): Resource[F, String] =
    f(toHttp4sApp(app, onError)).map(_.baseUri.show)

object Http4sServer:
  def apply[F[_]: Concurrent](f: Http4sApp[F] => Resource[F, Underlying]): Server[F] = new Http4sServer[F](f)
