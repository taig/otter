package io.taig.otter.sample.app

import cats.effect.IOApp
import cats.effect.IO
import io.taig.otter.sample.Librarian
import io.taig.otter.sample.api.Dsl.*
import io.taig.otter.server.Http4sServer
import org.http4s.ember.server.EmberServerBuilder
import org.typelevel.log4cats.noop.NoOpFactory
import org.typelevel.log4cats.LoggerFactory

object SampleApp extends IOApp.Simple:
  given LoggerFactory[IO] = NoOpFactory[IO]

  override def run: IO[Unit] = for
    repositories <- SampleRepositories()
    administrator = Librarian.Create.Default
    _ <- repositories.librarian.create(administrator).rethrow
    login = administrator.toLibrarianLogin
    session <- repositories.librarian.login(login).rethrow
    _ <- IO.println(s"Created administrator account: ${login.email}:${login.password} ($session)")
    implementation = SampleEndpointImplementation(repositories.librarian)
    server = Http4sServer[IO](EmberServerBuilder.default[IO].withHttpApp(_).build)
    routes = SampleRoutes(implementation, repositories)
    _ <- server.start(app(routes), throwable => IO(throwable.printStackTrace())).useForever
  yield ()
