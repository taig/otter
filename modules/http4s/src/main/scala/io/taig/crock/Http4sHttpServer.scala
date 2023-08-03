package io.taig.crock

import cats.data.{Chain, OptionT, Validated}
import cats.effect.Async
import cats.syntax.all.*
import fs2.io.net.Network
import io.taig.crock.http.{HttpDecoder, HttpServer, Method, Routes}
import io.taig.crock.schema.Violations
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.{HttpRoutes, Request}
import org.typelevel.log4cats.LoggerFactory

final class Http4sHttpServer[F[+_]: Async: Network: LoggerFactory] extends HttpServer[F, Request[F]]:
  override def start(routes: Routes[F]): F[Unit] =
    val httpRoutes = HttpRoutes[F]: request =>
      val path = Chain.fromSeq(request.uri.path.segments.map(_.decoded()))
      val queries = Chain.fromSeq(request.uri.query.toVector).mapFilter { case (name, value) => value.tupleLeft(name) }

      routes.filter(Method(request.method.name), path, queries).toNec match {
        case Some(routes) =>
          val x: Chain[Validated[Violations, Any]] = routes.toChain.map { route =>
            val url = route.endpoint.input.url
            HttpDecoder.url.decode(url, (path, queries))
          }
          ???
        case None => OptionT.none
      }

      ???

    EmberServerBuilder.default[F].withHttpApp(httpRoutes.orNotFound).build.useForever
