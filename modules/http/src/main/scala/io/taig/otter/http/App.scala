package io.taig.otter.http

import cats.syntax.all.*
import cats.MonadThrow
import org.typelevel.ci.*
import io.taig.otter.http.header.Accept
import io.taig.otter.Violations
import io.taig.otter.XPath
import io.taig.otter.Violation

final case class App[F[_]](routes: Routes[F], error: Results[Error[App.Error]]):
  def apply(request: Http.Request, onError: Throwable => F[Unit])(using F: MonadThrow[F]): F[Http.Response] =
    val accept = request.headers
      .collectFirst { case (ci"Accept", value) => value }
      .traverse: value =>
        Accept
          .parse(value)
          .toValidated
          .leftMap(_ => Violations.namespaceNec(XPath.Root / "header" / "Accept", Violation.tpe("rfc9110", value)))
      .map(_.map(_.toResult))

    routes.find(request.method, request.url) match
      case Some(route) =>
        accept
          .leftMap(Error(tpe = Route.Error.ValidationViolations, _))
          .fold(
            route.endpoint.response.error.encode(_).pure[F],
            route(_, request, onError)
          )
      case None =>
        val value = Error(App.Error.RouteNotFound)
        accept.toOption.flatten
          .flatMap(error.encode(_, value))
          .getOrElse(error.encode(value))
          .pure[F]

object App:
  enum Error:
    case RouteNotFound
