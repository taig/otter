package io.taig.otter.sample

import cats.effect.{IO, IOApp}
import io.taig.otter.dsl.*
import io.taig.otter.http.App
import io.taig.otter.sample.data.Librarian
import io.taig.otter.sample.service.{EndpointImplementation, ReferenceGenerator}
import io.taig.otter.typescript.*
import cats.data.Chain

object SampleApp extends IOApp.Simple:
  override def run: IO[Unit] = for
    // app <- create(IO.println)
    // server = Http4sHttpServer[IO](SampleServer.apply)
    // _ <- server.start(app)
    _ <- IO.println(
      toExportedTypes(Chain(api.codecs.book.main, api.codecs.librarian.summary, api.codecs.member.summary))
    )
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
