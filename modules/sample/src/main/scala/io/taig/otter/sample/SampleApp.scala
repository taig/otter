package io.taig.otter.sample

import cats.effect.{IO, IOApp}
import io.taig.otter.http.App
import io.taig.otter.sample.data.Librarian
import cats.data.Chain

object SampleApp extends IOApp.Simple:
  override def run: IO[Unit] = for
    // app <- create(IO.println)
    // server = Http4sHttpServer[IO](SampleServer.apply)
    // _ <- server.start(app)
    // _ <- IO.println(
    //   toExportedTypes(Chain(api.codecs.book.main, api.codecs.librarian.summary, api.codecs.member.summary))
    // )
    _ <- IO.unit
  yield ()

  def create(logger: String => IO[Unit]): IO[App[IO]] = for
    repositories <- SampleRepositories()
    administrator = Librarian.Create.Default
    _ <- repositories.librarian.create(administrator).rethrow
    login = administrator.toLogin
    session <- repositories.librarian.login(login).rethrow
    _ <- logger(s"Created initial librarian account: ${login.email}:${login.password} ($session)")
  // implementation = new EndpointImplementation(repositories.librarian)
  // routes = SampleRoutes(implementation, repositories)
  yield ??? // app(routes)
