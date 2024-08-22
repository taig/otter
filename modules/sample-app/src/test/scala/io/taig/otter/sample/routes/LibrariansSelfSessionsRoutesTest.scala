package io.taig.otter.sample.routes

import io.taig.otter.sample.api.endpoint
import io.taig.otter.sample.SampleSuite
import io.taig.otter.sample.api.schema.LibrarianApiSchema
import io.taig.otter.sample.Librarian
import io.github.arainko.ducktape.*

final class LibrariansSelfSessionsRoutesTest extends SampleSuite:
  app.test(endpoint.librarians.self.sessions.post()): context =>
    for _ <- context.client
        .submit(
          endpoint.librarians.self.sessions.post(),
          Librarian.Create.Default.toLibrarianLogin.to[LibrarianApiSchema.Login]
        )
        .assertSuccess
    yield {}

//   app.test(endpoints.librarians.self.sessions.post, description = "email unknown"): context =>
//     val login = Librarian.Login(email = ci"foo@bar", password = "")
//     for obtained <- context.client.submit(endpoints.librarians.self.sessions.post, login).assertError
//     yield {
//       assertEquals(obtained, Post.EmailOrPasswordIncorrect)
//     }

//   app.test(endpoints.librarians.self.sessions.post, description = "password incorrect"): context =>
//     val login = Librarian.Login(email = Librarian.Create.Default.email.toCIString, password = "")
//     for obtained <- context.client.submit(endpoints.librarians.self.sessions.post, login).assertError
//     yield {
//       assertEquals(obtained, Post.EmailOrPasswordIncorrect)
//     }
