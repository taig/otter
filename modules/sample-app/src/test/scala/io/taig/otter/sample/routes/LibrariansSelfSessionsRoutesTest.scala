package io.taig.otter.sample.routes

import io.taig.otter.sample.api.endpoint
import io.taig.otter.sample.SampleSuite
import io.taig.otter.sample.api.schema.LibrarianApiSchema
import io.taig.otter.sample.Librarian
import io.github.arainko.ducktape.*
import org.typelevel.ci.*
import cats.syntax.all.*
import io.taig.otter.sample.api.endpoint.librarians.self.sessions.post.Error

final class LibrariansSelfSessionsRoutesTest extends SampleSuite:
  app.test(endpoint.librarians.self.sessions.post()): context =>
    for _ <- context.client
        .fallible(
          endpoint.librarians.self.sessions.post(),
          session = none,
          Librarian.Create.Default.toLibrarianLogin.to[LibrarianApiSchema.Login]
        )
        .assertSuccess
    yield {}

  app.test(endpoint.librarians.self.sessions.post(), description = "email unknown"): context =>
    val login = Librarian.Login(email = ci"foo@bar", password = "password")
    for obtained <- context.client
        .fallible(endpoint.librarians.self.sessions.post(), session = none, login.to[LibrarianApiSchema.Login])
        .assertError
    yield {
      assertEquals(obtained, expected = Error.EmailOrPasswordIncorrect)
    }

  app.test(endpoint.librarians.self.sessions.post(), description = "password incorrect"): context =>
    val login = Librarian.Login(email = Librarian.Create.Default.email, password = "")
    for obtained <- context.client
        .fallible(endpoint.librarians.self.sessions.post(), session = none, login.to[LibrarianApiSchema.Login])
        .assertError
    yield {
      assertEquals(obtained, Error.EmailOrPasswordIncorrect)
    }
