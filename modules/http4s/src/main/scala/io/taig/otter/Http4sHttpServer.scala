package io.taig.otter

import cats.ApplicativeThrow
import cats.data.{Chain, OptionT, Validated}
import cats.effect.Async
import cats.syntax.all.*
import fs2.io.net.Network
import io.taig.otter.http.*
import io.taig.otter.schema.Violations
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.{
  Entity,
  Header as Http4sHeader,
  Headers as Http4sHeaders,
  HttpRoutes,
  Request as Http4sRequest,
  Response as Http4sResponse,
  Status
}
import org.typelevel.log4cats.LoggerFactory

final class Http4sHttpServer[F[+_]: Async: Network: LoggerFactory] extends HttpServer[F, Http4sRequest[F]]:
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
          val result = routes.foldLeftM(Chain.empty[(Url[?], Violations)].asLeft[Http.Response]):
            case (result @ Right(_), _) => result.pure[F]
            case (Left(failures), route) =>
              Fs2HttpDecoder
                .decode(route.endpoint.request.body, request.body)
                .map(Http.Request(method, url, headers, _))
                .map(HttpDecoder.request.decode(route.endpoint.request, _))
                .flatMap:
                  case Validated.Valid(a) =>
                    route.implementation(a).map(HttpEncoder.response.encode(route.endpoint.response, _)).map(_.asRight)
                  case Validated.Invalid(violations) => Left(failures :+ (route.endpoint.request.url, violations)).pure

          OptionT
            .liftF(result)
            .semiflatMap:
              case Right(response) => toHttp4sResponse(response)
              case Left(failures)  => ???
        case None => OptionT.none

    EmberServerBuilder.default[F].withHttpApp(httpRoutes.orNotFound).build.useForever

  def toHttp4sHeaders(headers: Http.Headers): Http4sHeaders =
    new Http4sHeaders(headers.toList.map(Http4sHeader.Raw.apply.tupled))

  def toHttp4sResponse(response: Http.Response): F[Http4sResponse[F]] = for
    status <- Status.fromInt(response.code.toInt).liftTo[F]
    headers = toHttp4sHeaders(response.headers)
    body <- ApplicativeThrow[F].catchOnly[ClassCastException](response.body.entity.asInstanceOf[Fs2Stream[F]].toFs2)
  yield Http4sResponse(status, headers = headers, entity = Entity.stream(body))
