package io.taig.otter

import cats.data.{Chain, OptionT, Validated}
import cats.effect.Async
import cats.syntax.all.*
import fs2.Stream
import fs2.io.net.Network
import io.taig.otter.http.Input.Body
import io.taig.otter.http.{Endpoint, Http, HttpDecoder, HttpEncoder, HttpServer, Input, Method, Output, Routes}
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
          val result = routes.foldLeft(Chain.empty[(Endpoint[?, ?], Violations)].asLeft[F[Http.Response]]):
            case (result @ Right(_), _) => result
            case (Left(failures), route) =>
              val endpoint = route.endpoint
              HttpDecoder.url.decode(endpoint.input.url, url) match {
                case Validated.Valid(_) =>
                  decode(route.endpoint.input.body, request.entity.body)
                  route.implementation(???).flatMap(b => encode(route.endpoint.output, b))
                  // Right(route.implementation(a).map(a => HttpEncoder.output.encode(route.endpoint.output, a)))
                  ???
                case Validated.Invalid(violations) => Left(failures :+ (endpoint, violations))
              }
          result match
            case Right(response) => OptionT.liftF(response.flatMap(toHttp4sResponse))
            case Left(failures)  => ??? // TODO 404 with tried routes
        case None => OptionT.none

    EmberServerBuilder.default[F].withHttpApp(httpRoutes.orNotFound).build.useForever

  // TODO generalize into reusable Decoder
  def decode[A](body: Input.Body[A], data: Stream[F, Byte]): F[Validated[Violations, A]] = body match
    case Input.Body.Singlepart.Strict.Bytes => data.compile.to(Array).map(_.valid)
    case Input.Body.Singlepart.Strict.Validate(self, validation, _) =>
      decode(self, data).map(_.andThen(validation(_).leftMap(Violations.root)))

  def encode[A](output: Output[A], a: A): F[Http.Response] = ???
