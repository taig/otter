package io.taig.otter.sample.app

import cats.effect.kernel.Resource

import cats.effect.IO
import cats.effect.ResourceApp
import org.http4s.ember.server.EmberServerBuilder
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.NoOpFactory
import org.typelevel.log4cats.slf4j.Slf4jLogger
import io.taig.otter.http.*
import io.circe.Printer
import org.typelevel.log4cats.slf4j.Slf4jFactory

object SampleApp extends ResourceApp.Forever:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  override def run(args: List[String]): Resource[IO, Unit] = for
    routes <- Resource.pure(SampleRoutes())
    encoder = CirceJsonBodyEncoder(printer = Printer.noSpaces)
    http4s = toHttp4sRoutes(routes, encoder = encoder)
    server <- EmberServerBuilder.default[IO].withHttpApp(http4s.orNotFound).build
  yield ()
