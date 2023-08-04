package io.taig.otter

import cats.data.{Chain, OptionT, Validated}
import cats.effect.Async
import cats.syntax.all.*
import fs2.io.net.Network
import io.taig.otter.http.{Code, Endpoint, Http, HttpDecoder, HttpEncoder, HttpServer, Method, Routes}
import io.taig.otter.schema.Violations
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.{HttpRoutes, Request as Http4sRequest, Response, Status}
import org.typelevel.log4cats.LoggerFactory

final class Http4sHttpServer[F[+_]: Async: Network: LoggerFactory] extends HttpServer[F, Http4sRequest[F]]:
  def toRequest(request: Http4sRequest[F]): Http.Request = Http.Request(
    Method(request.method.name),
    Http.Url(
      Chain.fromSeq(request.uri.path.segments.map(_.decoded())),
      Chain.fromSeq(request.uri.query.toVector).mapFilter { case (name, value) => value.tupleLeft(name) }
    ),
    headers = ???,
    body = ???
  )

  override def start(routes: Routes[F]): F[Unit] =
    val httpRoutes = HttpRoutes[F]: request =>
      routes.filter(toRequest(request)).toNec match
        case Some(routes) =>
          val result = routes.foldLeft(Chain.empty[(Endpoint[?, ?], Violations)].asLeft[F[Code]]):
            case (result @ Right(_), _) => result
            case (Left(failures), route) =>
              val endpoint = route.endpoint
              HttpDecoder.input.decode(endpoint.input, ???) match
                case Validated.Valid(a) =>
                  Right(route.implementation(a).map(a => HttpEncoder.output.encode(route.endpoint.output, a)))
                case Validated.Invalid(violations) => Left(failures :+ (endpoint, violations))
          result match
            case Right(response) =>
              OptionT.liftF(response).map { code =>
                Response(status = Status.fromInt(code.toInt).toOption.getOrElse(Status.Ok))
              }
            case Left(failures) => ??? // TODO 404 with tried routes
        case None => OptionT.none

    EmberServerBuilder.default[F].withHttpApp(httpRoutes.orNotFound).build.useForever
