package io.taig.otter.http

import cats.syntax.all.*
import cats.MonadThrow
import org.typelevel.ci.*
import io.taig.otter.http.header.Accept
import io.taig.otter.Violations
import io.taig.otter.XPath
import io.taig.otter.Violation
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.Parameters
import cats.data.Ior
import cats.data.NonEmptyList

final case class App[F[_]](routes: Routes[F], notFound: Response[Unit]):
  def apply(request: Http.Request, onError: Throwable => F[Unit])(using F: MonadThrow[F]): F[Http.Response] =
    val accept = request.headers
      .collectFirst { case (ci"Accept", value) => value }
      .traverse: value =>
        Accept
          .parse(value)
          .toValidated
          .leftMap(_ => Violations.namespaceNec(XPath.Root / "header" / "Accept", Violation.tpe("accept", value)))
      .map(_.map(_.toResult))

    routes.find(request.method, request.url) match
      case Some(route) =>
        // accept.fold(
        //   route.endpoint.response.validationViolations.encode(none, _).pure[F],
        //   route(_, request, onError)
        // )
        ???
      case None =>
        val wildcard = MediaRange(MediaRange.Type.Any, Parameters.Empty)
        val acceptOrWildcard = accept.fold(
          _ => Ior.right(NonEmptyList.one(wildcard)),
          {
            case Some(Ior.Left(blocklist))             => Ior.Both(blocklist, NonEmptyList.one(wildcard))
            case Some(Ior.Right(acceptlist))           => Ior.Right(acceptlist :+ wildcard)
            case Some(Ior.Both(blocklist, acceptlist)) => Ior.Both(blocklist, acceptlist :+ wildcard)
            case None                                  => Ior.right(NonEmptyList.one(wildcard))
          }
        )

        // notFound.encode(acceptOrWildcard.some, Request.Result.Success(())).pure[F]
        ???
