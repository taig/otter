package io.taig.otter.sample.app

import cats.effect.IOApp
import cats.effect.IO
import io.taig.otter.sample.Librarian
import io.taig.otter.sample.api.Dsl.*

object SampleApp extends IOApp.Simple:
  override def run: IO[Unit] = for
    repositories <- SampleRepositories()
    administrator = Librarian.Create.Default
    _ <- repositories.librarian.create(administrator).rethrow
    login = administrator.toLibrarianLogin
    session <- repositories.librarian.login(login).rethrow
    _ <- IO.println(s"Created administrator account: ${login.email}:${login.password} ($session)")
    implementation = SampleEndpointImplementation(repositories.librarian)
    routes = SampleRoutes(implementation, repositories)
    _ = app(routes)
  yield ()
