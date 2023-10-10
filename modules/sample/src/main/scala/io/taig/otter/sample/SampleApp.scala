package io.taig.otter.sample

import cats.effect.{IO, IOApp}
import io.taig.otter.dsl.*
import io.taig.otter.http.App
import io.taig.otter.http4s.Http4sHttpServer
import io.taig.otter.sample.data.Librarian
import io.taig.otter.sample.service.{EndpointImplementation, ReferenceGenerator}

object SampleApp extends IOApp.Simple:
  override def run: IO[Unit] = for
    app <- create(IO.println)
    server = Http4sHttpServer[IO](SampleServer.apply)
    _ <- server.start(app)
  yield ()

  def create(logger: String => IO[Unit]): IO[App[IO]] = for
    repositories <- SampleRepositories(ReferenceGenerator())
    administrator = Librarian.Create.Default
    _ <- repositories.librarian.create(administrator).rethrow
    login = administrator.toLogin
    session <- repositories.librarian.login(login).rethrow
    _ <- logger(s"Created librarian account: ${login.email}:${login.password} ($session)")
    implementation = new EndpointImplementation(repositories.librarian)
    routes = SampleRoutes(implementation, repositories)
  yield app(routes)
