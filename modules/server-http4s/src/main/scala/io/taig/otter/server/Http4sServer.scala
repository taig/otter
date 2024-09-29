package io.taig.otter.server

import cats.effect.Concurrent
import cats.effect.Resource
import cats.syntax.all.*
import io.taig.otter.http.*
import io.taig.otter.http.http4s.*
import org.http4s.HttpApp as Http4sApp
import org.http4s.server.Server as Underlying

final class Http4sServer[F[_]: Concurrent](f: Http4sApp[F] => Resource[F, Underlying], onError: Throwable => F[Unit])
    extends Server[F]:
  override def start(app: App[F]): Resource[F, String] = f(toHttp4sApp(app, onError)).map(_.baseUri.show)

object Http4sServer:
  def apply[F[_]: Concurrent](f: Http4sApp[F] => Resource[F, Underlying], onError: Throwable => F[Unit]): Server[F] =
    new Http4sServer[F](f, onError)
