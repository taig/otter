package io.taig.otter.sample

import cats.effect.{IO, IOApp}
import io.taig.otter.dsl.*
import io.taig.otter.http4s.Http4sHttpServer
import io.taig.otter.sample.data.Librarian
import io.taig.otter.sample.service.ReferenceGenerator
import org.typelevel.ci.CIStringSyntax
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

object SampleApp extends IOApp.Simple:
  override def run: IO[Unit] =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val references = ReferenceGenerator()

    for
      repositories <- SampleRepositories(references)
      administrator = Librarian.Create(
        Librarian.Email.unsafeFromCIString(ci"me@otter.org"),
        Librarian.Password.unsafeFromString("password")
      )
      _ <- repositories.librarian.create(administrator).rethrow
      login = administrator.toLogin
      session <- repositories.librarian.login(login).rethrow
      _ <- IO.println(s"Created librarian account: ${login.email}:${login.password} ($session)")
      route = new SampleRoute(repositories.librarian)
      routes = SampleRoutes(route, repositories)
      server = new Http4sHttpServer[IO]
      _ <- server.start(app(routes))
    yield ()
