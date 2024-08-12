package io.taig.otter.sample.app

import cats.effect.IO
import io.taig.otter.sample.Librarian
import io.taig.otter.sample.api.Dsl.*
import io.taig.otter.server.Http4sServer
import org.http4s.ember.server.EmberServerBuilder
import org.typelevel.log4cats.noop.NoOpFactory
import org.typelevel.log4cats.LoggerFactory
import cats.effect.ResourceApp
import cats.effect.kernel.Resource

object SampleApp extends ResourceApp.Forever:
  given LoggerFactory[IO] = NoOpFactory[IO]

  override def run(args: List[String]): Resource[IO, Unit] = for
    repositories <- Resource.eval(SampleRepositories())
    administrator = Librarian.Create.Default
    _ <- Resource.eval(repositories.librarian.create(administrator).rethrow)
    login = administrator.toLibrarianLogin
    session <- Resource.eval(repositories.librarian.login(login).rethrow)
    _ <- Resource.eval(IO.println(s"Created administrator account: ${login.email}:${login.password} ($session)"))
    implementation = SampleEndpointImplementation(repositories.librarian)
    server = Http4sServer[IO](EmberServerBuilder.default[IO].withHttpApp(_).build)
    routes = SampleRoutes(implementation, repositories)
    _ <- server.start(app(routes), throwable => IO(throwable.printStackTrace()))
  yield ()
