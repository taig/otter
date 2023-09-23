package io.taig.otter.sample

import cats.effect.{IO, IOApp}
import io.taig.otter.http.Route
import io.taig.otter.http4s.Http4sHttpServer
import io.taig.otter.dsl.*
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

object SampleApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val routes = Route(endpoints.books.post, IO.pure).toRoutes

    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    val server = new Http4sHttpServer[IO]

    server.start(app(routes))
