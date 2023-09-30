package io.taig.otter.sample.routes

import io.taig.otter.sample.SampleSuite
import io.taig.otter.sample.api.endpoints
import io.taig.otter.sample.api.endpoints.librarians.self.sessions.Post
import io.taig.otter.sample.data.Librarian
import org.typelevel.ci.*

final class LibrariansSelfSessionsRoutesTest extends SampleSuite:
  app.test(endpoints.librarians.self.sessions.post): context =>
    for _ <- context.client.submitSuccess(
        endpoints.librarians.self.sessions.post,
        session = None,
        Librarian.Create.Default.toLogin
      )
    yield {}

  app.test(endpoints.librarians.self.sessions.post, description = "email unknown"): context =>
    val login = Librarian.Login(email = ci"foo@bar", password = "")
    for obtained <- context.client.submitError(endpoints.librarians.self.sessions.post, session = None, login)
    yield {
      assertEquals(obtained, Post.EmailOrPasswordIncorrect)
    }
