package io.taig.otter.sample.routes

import cats.effect.IO
import io.taig.otter.http.Routes
import io.taig.otter.sample.SampleRoute
import io.taig.otter.sample.api.endpoints.librarians.self.sessions.Post
import io.taig.otter.sample.api.{endpoints, Route}
import io.taig.otter.sample.data.{Librarian, Session}
import io.taig.otter.sample.repository.LibrarianRepository
import io.taig.otter.sample.repository.LibrarianRepository.Error
import mouse.all.*

final class LibrariansSelfSessionsRoutes(route: SampleRoute, librarian: LibrarianRepository):
  val post: Route[Librarian.Login, Either[Post, Session]] =
    route(endpoints.librarians.self.sessions.post): (_, login) =>
      librarian
        .login(login)
        .leftMapIn:
          case Error.Login.EmailUnknown      => Post.EmailOrPasswordIncorrect
          case Error.Login.PasswordIncorrect => Post.EmailOrPasswordIncorrect

object LibrariansSelfSessionsRoutes:
  def apply(route: SampleRoute, librarian: LibrarianRepository): Routes[IO] =
    val routes = new LibrariansSelfSessionsRoutes(route, librarian)
    Routes(routes.post)
