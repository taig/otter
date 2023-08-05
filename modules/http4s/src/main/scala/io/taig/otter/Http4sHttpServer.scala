package io.taig.otter

import cats.data.{Chain, OptionT, Validated}
import cats.effect.Async
import cats.syntax.all.*
import fs2.Stream
import fs2.io.net.Network
import io.taig.otter.http.Request.Body
import io.taig.otter.http.{Endpoint, Http, HttpDecoder, HttpEncoder, HttpServer, Method, Request, Response, Routes, Url}
import io.taig.otter.schema.Violations
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.{HttpRoutes, Request as Http4sRequest, Response as Http4sResponse}
import org.typelevel.log4cats.LoggerFactory

final class Http4sHttpServer[F[+_]: Async: Network: LoggerFactory] extends HttpServer[F, Http4sRequest[F]]:
  def toHttp4sResponse(response: Http.Response): F[Http4sResponse[F]] = ???

  override def start(routes: Routes[F]): F[Unit] =
    val httpRoutes = HttpRoutes[F]: request =>
      val method = Method(request.method.name)
      val url = Http.Url(
        Chain.fromSeq(request.uri.path.segments.map(_.decoded())),
        Chain.fromSeq(request.uri.query.toVector).mapFilter { case (name, value) => value.tupleLeft(name) }
      )

      val headers = Chain.fromSeq(request.headers.headers.map(header => header.name -> header.value))

      routes.filter(method, url).toNec match
        case Some(routes) =>
          val result = routes.foldLeft(Chain.empty[(Url[?], Violations)].asLeft[F[Http.Response]]):
            case (result @ Right(_), _) => result
            case (Left(failures), route) =>
              val request = route.endpoint.request
              HttpDecoder.request[F].decode(request, ???).flatMap { result =>
                result.traverse(a => route.implementation(a).flatMap(b => encode(route.endpoint.response, b)))
              }
              HttpDecoder.url.decode(request.url, url) match {
                case Validated.Valid(_) =>
//                  Http4sHttpDecoder.body.decode(route.endpoint.request.body, request.entity.body)
                  route.implementation(???).flatMap(b => encode(route.endpoint.response, b))
                  // Right(route.implementation(a).map(a => HttpEncoder.output.encode(route.endpoint.output, a)))
                  ???
                case Validated.Invalid(violations) => Left(failures :+ (request.url, violations))
              }
          result match
            case Right(response) => OptionT.liftF(response.flatMap(toHttp4sResponse))
            case Left(failures)  => ??? // TODO 404 with tried routes
        case None => OptionT.none

    EmberServerBuilder.default[F].withHttpApp(httpRoutes.orNotFound).build.useForever

  def encode[A](output: Response[A], a: A): F[Http.Response] = ???
