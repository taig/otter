package io.taig.otter.sample.app

import cats.effect.IO
import cats.effect.Resource
import cats.effect.ResourceApp
import cats.effect.kernel.Ref
import io.circe.Printer
import io.taig.otter.+
import io.taig.otter.Json
import io.taig.otter.http.*
import io.taig.otter.sample.api.schema.librarian.LibrarianApiSchema
import org.http4s.ember.server.EmberServerBuilder
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

object SampleApp extends ResourceApp.Forever:
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  override def run(args: List[String]): Resource[IO, Unit] = for
    routes <- Resource.eval(routes)
    decoder = CirceJsonPayloadDecoder.Default.or(FormDataPayloadDecoder.Default)
    encoder = CirceJsonPayloadEncoder(printer = Printer.noSpaces).or(FormDataPayloadEncoder.Default)
    http4s = toHttp4sRoutes(routes, decoder, encoder)
    _ <- EmberServerBuilder.default[IO].withHttpApp(http4s.orNotFound).build
  yield ()

  val routes = Ref[IO].empty[List[LibrarianApiSchema]].map(SampleRoutes.apply)
