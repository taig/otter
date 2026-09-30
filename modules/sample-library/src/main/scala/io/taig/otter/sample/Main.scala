package io.taig.otter.sample

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import com.comcast.ip4s.host
import com.comcast.ip4s.port
import org.http4s.ember.server.EmberServerBuilder
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.NoOpFactory

/** The library, listening.
  *
  * This is the one thing `otter-http-http4s` deliberately does not do. It hands back an `HttpApp` and stops, because
  * what to listen on is the caller's -- and here the caller is fifteen lines. What a `404` looks like is not: the API
  * declares it, and the `405` beside it is an answer only the router could give. Mounting something else beside these
  * routes is `Http4s.routes` and `Http4s.fallback` composed with `<+>` instead.
  */
object Main extends IOApp:
  override def run(arguments: List[String]): IO[ExitCode] = Library[IO]()
    .flatMap: library =>
      given LoggerFactory[IO] = NoOpFactory[IO]

      EmberServerBuilder
        .default[IO]
        .withHost(host"0.0.0.0")
        .withPort(port"8080")
        .withHttpApp(LibraryRoutes(library))
        .build
        .use(server => IO.println(s"Listening on ${server.baseUri}") *> IO.never)
    .as(ExitCode.Success)
