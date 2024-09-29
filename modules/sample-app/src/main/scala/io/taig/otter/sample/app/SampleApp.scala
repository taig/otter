package io.taig.otter.sample.app

import cats.effect.IO
import cats.effect.ResourceApp
import cats.effect.kernel.Resource
import io.taig.otter.sample.Librarian
import io.taig.otter.sample.api.Dsl.*
import io.taig.otter.server.Http4sServer
import org.http4s.ember.server.EmberServerBuilder
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.NoOpFactory

object SampleApp extends ResourceApp.Forever:
  given LoggerFactory[IO] = NoOpFactory[IO]

  override def run(args: List[String]): Resource[IO, Unit] = for
    app <- Resource.eval(SampleApp(IO.println))
    server = Http4sServer[IO](
      EmberServerBuilder.default[IO].withHttpApp(_).build,
      onError = throwable => IO(throwable.printStackTrace())
    )
    _ <- server.start(app)
  yield ()

  def apply(logger: String => IO[Unit]): IO[App[IO]] = for
    repositories <- SampleRepositories()
    administrator = Librarian.Create.Default
    _ <- repositories.librarian.create(administrator).rethrow
    login = administrator.toLibrarianLogin
    session <- repositories.librarian.login(login).rethrow
    _ <- logger(s"Created administrator account: ${login.email}:${login.password} ($session)")
    implementation = SampleEndpointImplementation(repositories.librarian)
    routes = SampleRoutes(implementation, repositories)
  yield app(routes)
