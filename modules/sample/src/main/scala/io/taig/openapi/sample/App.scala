package io.taig.openapi.sample

import cats.effect.{IO, IOApp}
import io.taig.openapi.http4s.*
import org.http4s.{Response, Status}
import org.http4s.ember.server.EmberServerBuilder

object App extends IOApp.Simple:
  override def run: IO[Unit] = PetRoutes.empty(Authentication.default).map(toHttp4sRoutes[IO]).flatMap { routes =>
    EmberServerBuilder
      .default[IO]
      .withHttpApp(routes.orNotFound)
      .withErrorHandler { throwable =>
        IO(throwable.printStackTrace()) *>
          IO(Response(Status.InternalServerError).withEntity("Something went terribly wrong"))
      }
      .build
      .useForever
  }
