package io.taig.otter.http

import cats.syntax.all.*
import cats.MonadThrow
import org.typelevel.ci.*
import io.taig.otter.http.header.Accept
import cats.Show

final case class App[F[_]](routes: Routes[F], error: Results[App.Error]):
  def apply(request: Http.Request, onError: Throwable => F[Unit])(using F: MonadThrow[F]): F[Http.Response] =
    routes.find(request.method, request.url) match
      case Some(route) => route(request, onError)
      case None =>
        val accept = request.headers
          .collectFirst { case (ci"Accept", value) => value }
          .flatMap(Accept.parse(_).toOption)
          .map(_.toResult)

        accept
          .flatMap(error.encode(_, App.Error.RouteNotFound))
          // TODO charset
          .getOrElse(error.encode(charset = none, App.Error.RouteNotFound))
          .pure[F]

object App:
  enum Error:
    case RouteNotFound

  object Error:
    def parse(value: String): Option[App.Error.RouteNotFound.type] =
      Parsers.error.parseAll(value).toOption.filter(_ === "routeNotFound").as(RouteNotFound)

    given Show[App.Error] =
      case RouteNotFound => Printers.error(name = "routeNotFound")
