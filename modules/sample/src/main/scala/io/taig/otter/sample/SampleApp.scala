package io.taig.otter.sample

import cats.data.Chain
import cats.effect.std.AtomicCell
import cats.syntax.all.*
import cats.effect.{IO, IOApp}
import io.taig.otter.dsl.*
import io.taig.otter.http4s.Http4sHttpServer
import io.taig.otter.sample.api.{Book, Librarian}
import io.taig.otter.sample.service.ReferenceGenerator
import org.typelevel.ci.CIStringSyntax
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.slf4j.Slf4jFactory

object SampleApp extends IOApp.Simple:
  override def run: IO[Unit] =
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val references = ReferenceGenerator()

    for
      books <- AtomicCell[IO].empty[Chain[Book]]
      librarians <- AtomicCell[IO].empty[Chain[Librarian]]
      repositories = SampleRepositories(references, books, librarians)
      administrator = Librarian.Create(
        Librarian.Email.unsafeFromCIString(ci"me@otter.org"),
        Librarian.Password.unsafeFromString("password")
      )
      administrator <- repositories.librarian.create(administrator).rethrow
      _ <- IO.println(s"Created administrator account: $administrator")
      route = new SampleRoute(librarians)
      routes = SampleRoutes(route, repositories)
      server = new Http4sHttpServer[IO]
      _ <- server.start(app(routes))
    yield ()
