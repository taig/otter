package io.taig.otter.sample.app

import cats.effect.Resource

import cats.effect.IO
import cats.effect.ResourceApp
import org.http4s.ember.server.EmberServerBuilder
import org.typelevel.log4cats.LoggerFactory
import io.taig.otter.http.*
import io.circe.Printer
import org.typelevel.log4cats.slf4j.Slf4jFactory
import io.taig.otter.http.header.MediaType
import io.taig.otter.+
import io.taig.otter.Json

object SampleApp extends ResourceApp.Forever:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  override def run(args: List[String]): Resource[IO, Unit] = for
    routes <- Resource.pure(SampleRoutes())
    decoder = CirceJsonPayloadDecoder.Default.or(FormDataPayloadDecoder.Default)
    encoder = CirceJsonBodyEncoder(printer = Printer.noSpaces).or(FormDataBodyEncoder.Default)
    http4s = toHttp4sRoutes(routes, decoder, encoder)
    server <- EmberServerBuilder.default[IO].withHttpApp(http4s.orNotFound).build
  yield ()
