package io.taig.openapi.sample

import cats.syntax.all.*
import cats.effect.{IO, IOApp, Resource, ResourceApp}
import org.http4s.HttpRoutes
import org.http4s.syntax.all.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import fs2.{Chunk, Stream}

object SampleApp extends ResourceApp.Forever {
  val chunk = Chunk("lorem", "ipsum", "dolar", "sit", "amet")
  val data = Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk) ++
    Stream.chunk(chunk)
  
  val routes: HttpRoutes[IO] = HttpRoutes.of { case GET -> Root =>
    Ok(data)
  }

  override def run(args: List[String]): Resource[IO, Unit] =
    EmberServerBuilder
      .default[IO]
      .withHttpApp(routes.orNotFound)
      .build
      .void
}
