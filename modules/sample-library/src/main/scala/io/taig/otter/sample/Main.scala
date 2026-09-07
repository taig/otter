package io.taig.otter.sample

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import com.comcast.ip4s.host
import com.comcast.ip4s.port
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits.*
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.NoOpFactory

/** The library, listening.
  *
  * This is the one thing `otter-http-http4s` deliberately does not do. It hands back an `HttpRoutes` and stops, because
  * what to listen on, what a `404` looks like, and what else is mounted beside these routes are all the caller's -- and
  * here the caller is fifteen lines. `orNotFound` is where falling through stops being an option and becomes an answer.
  */
object Main extends IOApp:
  /** Ember asks for one, and this sample has nothing to say through it. It comes with `log4cats-core`, which arrives
    * with the server, so saying so costs no dependency.
    */
  private given LoggerFactory[IO] = NoOpFactory[IO]

  override def run(arguments: List[String]): IO[ExitCode] =
    Library[IO]()
      .flatMap: library =>
        EmberServerBuilder
          .default[IO]
          .withHost(host"0.0.0.0")
          .withPort(port"8080")
          .withHttpApp(LibraryRoutes(library).orNotFound)
          .build
          .use(server => IO.println(s"Listening on ${server.baseUri}") *> IO.never)
      .as(ExitCode.Success)
