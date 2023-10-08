package io.taig.otter.sample.routes

import io.taig.otter.sample.SampleSuite
import io.taig.otter.sample.api.endpoints
import io.taig.otter.sample.api.endpoints.librarians.self.sessions.Post
import io.taig.otter.sample.data.Librarian
import org.typelevel.ci.*

final class LibrariansSelfSessionsRoutesTest extends SampleSuite:
  app.test(endpoints.librarians.self.sessions.post): context =>
    for _ <- context.client
        .submit(
          endpoints.librarians.self.sessions.post,
          Librarian.Create.Default.toLogin
        )
        .assertSuccess
    yield {}

  app.test(endpoints.librarians.self.sessions.post, description = "email unknown"): context =>
    val login = Librarian.Login(email = ci"foo@bar", password = "")
    for obtained <- context.client.submit(endpoints.librarians.self.sessions.post, login).assertError
    yield {
      assertEquals(obtained, Post.EmailOrPasswordIncorrect)
    }

  app.test(endpoints.librarians.self.sessions.post, description = "password incorrect"): context =>
    val login = Librarian.Login(email = Librarian.Create.Default.email.toCIString, password = "")
    for obtained <- context.client.submit(endpoints.librarians.self.sessions.post, login).assertError
    yield {
      assertEquals(obtained, Post.EmailOrPasswordIncorrect)
    }
