package io.taig.otter.sample

import cats.effect.{IO, IOApp}
import io.taig.otter.dsl.*
import io.taig.otter.http4s.Http4sHttpServer
import io.taig.otter.sample.util.EndpointImplementation
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

object SampleApp extends IOApp.Simple:
  override def run: IO[Unit] =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]

    for
      repositories <- SampleRepositories()
      implementation = new EndpointImplementation()
      routes = SampleRoutes(implementation, repositories)
      server = new Http4sHttpServer[IO]
      _ <- server.start(app(routes))
    yield ()
