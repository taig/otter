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
import io.taig.otter.sample.app.conversion
import mouse.all.*

final class LibrariansSelfSessionsRoutes(implementation: EndpointImplementation[IO], librarian: LibrarianRepository):
  val post: AuthenticatedRoute[IO, LibrarianApiSchema.Login, Either[Error, SessionApiSchema]] =
    implementation(endpoint.librarians.self.sessions.post()): (_, login) =>
      librarian
        .login(conversion.toLibrarianLogin(login))
        .mapIn(conversion.toSessionApiSchema)
        .leftMapIn:
          case LibrarianRepository.Error.Login.EmailUnknown      => Error.EmailOrPasswordIncorrect
          case LibrarianRepository.Error.Login.PasswordIncorrect => Error.EmailOrPasswordIncorrect

object LibrariansSelfSessionsRoutes:
  def apply(implementation: EndpointImplementation[IO], librarian: LibrarianRepository): Routes[IO] =
    val routes = new LibrariansSelfSessionsRoutes(implementation, librarian)
    Routes(routes.post)
