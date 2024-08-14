package io.taig.otter.sample.app.routes

import cats.effect.IO
import io.taig.otter.http.Routes
import io.taig.otter.sample.api.EndpointImplementation
import io.taig.otter.sample.app.repository.LibrarianRepository
import io.taig.otter.sample.api.AuthenticatedRoute
import io.taig.otter.sample.api.schema.LibrarianApiSchema
import io.taig.otter.sample.api.endpoint.librarians.self.sessions.post.Error
import io.taig.otter.sample.api.endpoint
import io.taig.otter.sample.api.schema.SessionApiSchema
import io.taig.otter.sample.app.transformers.given
import mouse.all.*
import io.github.arainko.ducktape.*
import io.taig.otter.sample.Librarian

final class LibrariansSelfSessionsRoutes(implementation: EndpointImplementation[IO], librarian: LibrarianRepository):
  val post: AuthenticatedRoute[IO, LibrarianApiSchema.Login, Either[Error, SessionApiSchema]] =
    implementation(endpoint.librarians.self.sessions.post()): (_, login) =>
      librarian
        .login(login.to[Librarian.Login])
        .mapIn(_.to[SessionApiSchema])
        .leftMapIn:
          case LibrarianRepository.Error.Login.EmailUnknown      => Error.EmailOrPasswordIncorrect
          case LibrarianRepository.Error.Login.PasswordIncorrect => Error.EmailOrPasswordIncorrect

object LibrariansSelfSessionsRoutes:
  def apply(implementation: EndpointImplementation[IO], librarian: LibrarianRepository): Routes[IO] =
    val routes = new LibrariansSelfSessionsRoutes(implementation, librarian)
    Routes(routes.post)
