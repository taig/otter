package io.taig.crock

import cats.data.Chain
import cats.syntax.all.*
import io.taig.crock.http.{Endpoint, HttpServer, Method, Path, Routes, Segment}
import org.http4s.{Request, Uri}

final class Http4sHttpServer[F[_]] extends HttpServer[F, Request[F]]:
  override def start(routes: Routes[F]): F[Unit] =
    ???

    /*
    val matchingRoutes = findMatchingRoutes(
      Method(request.method.name),
      path = Chain.fromSeq(request.uri.path.segments.map(_.decoded())),
      queries = Chain.fromSeq(request.uri.query.toVector).mapFilter { case (name, value) => value.tupleLeft(name) },
      routes
    )
     */
