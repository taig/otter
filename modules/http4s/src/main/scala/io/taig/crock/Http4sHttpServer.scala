package io.taig.crock

import cats.data.{Chain, OptionT, Validated}
import cats.effect.Async
import cats.syntax.all.*
import fs2.io.net.Network
import io.taig.crock.http.{HttpServer, Method, Routes, Url}
import io.taig.crock.schema.Violations
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.{HttpRoutes, Request, Uri}
import org.typelevel.log4cats.LoggerFactory

final class Http4sHttpServer[F[+_]: Async: Network: LoggerFactory] extends HttpServer[F, Request[F]]:
  override def start(routes: Routes[F]): F[Unit] =
    val httpRoutes = HttpRoutes[F]: request =>
      val matchingRoutes = findMatchingRoutes(
        Method(request.method.name),
        path = Chain.fromSeq(request.uri.path.segments.map(_.decoded())),
        queries = Chain.fromSeq(request.uri.query.toVector).mapFilter { case (name, value) => value.tupleLeft(name) },
        routes
      )

      if matchingRoutes.isEmpty then OptionT.none else ???

      matchingRoutes.map { route =>
        val url = route.endpoint.input.url
        val path = Chain.fromSeq(request.uri.path.segments.map(_.decoded()))
        val queries =
          Chain.fromSeq(request.uri.query.toVector).mapFilter { case (name, value) => value.tupleLeft(name) }
        decodeUrl(path, queries, url)
        ???
      }

      ???

    EmberServerBuilder.default[F].withHttpApp(httpRoutes.orNotFound).build.useForever

  def decodeUrl[A](path: Chain[String], queries: Chain[(String, String)], url: Url[A]): Validated[Violations, A] =
    url match
      case Url.FromPath(path)       => ???
      case Url.FromQueries(queries) => ???
      case Url.Zip(left, right)     => ???
      case Url.Modify(self, f, _)   => decodeUrl(path, queries, self).map(f)
      case Url.Empty                => ().valid

  def decodePath[A](path: Chain[String], url: Url[A]): Validated[Violations, A] = ???

  def decodeQueries[A](queries: Chain[(String, String)], url: Url[A]): Validated[Violations, A] = ???
