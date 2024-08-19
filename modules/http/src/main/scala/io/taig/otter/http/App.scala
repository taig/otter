package io.taig.otter.http

import cats.syntax.all.*
import cats.MonadThrow
import org.typelevel.ci.*
import io.taig.otter.http.header.Accept

final case class App[F[_]](routes: Routes[F], error: Results[App.Error]):
  def apply(request: Http.Request, onError: Throwable => F[Unit])(using F: MonadThrow[F]): F[Http.Response] =
    try {
      routes.find(request.method, request.url) match
        case Some(route) => route(request, onError)
        case None =>
          val accept = request.headers
            .collectFirst { case (ci"Accept", value) => value }
            .flatMap(Accept.parse(_).toOption)
            .map(_.toResult)

          accept
            .flatMap(error.encode(_, App.Error.RouteNotFound))
            .getOrElse(error.encode(App.Error.RouteNotFound))
            .pure[F]
    } catch {
      case throwable: Throwable =>
        throwable.printStackTrace()
        ???
    }

object App:
  enum Error:
    case RouteNotFound
