package io.taig.otter.sample

import cats.effect.IO
import cats.syntax.all.*
import io.taig.otter.http.Route as OtterRoute
import io.taig.otter.sample.api.endpoints.{Authentication, Endpoint}
import io.taig.otter.sample.api.{Role, Route, Self, User}
import io.taig.otter.sample.data.{Librarian, Member}
import io.taig.otter.sample.repository.LibrarianRepository

import java.util.UUID

final class SampleRoute(library: LibrarianRepository):
  def apply[R <: Role, I, O](endpoint: Endpoint[R, I, O])(f: (Self[R], I) => IO[O]): Route[I, O] = OtterRoute(
    endpoint.toAuthenticatedEndpoint,
    authentication =>
      authentication.session
        .match {
          case Some(session) =>
            for
              user <- findUser(session).flatMap(_.liftTo[IO](Authentication.Error.UserUnknown))
              _ <- IO.raiseWhen(!endpoint.role.toSet.contains(user.toRole))(Authentication.Error.Forbidden)
              response <- f(user.asInstanceOf[Self[R]], authentication.payload)
            yield response
          case None if endpoint.role.toSet.contains(Role.Guest) =>
            f(().asInstanceOf[Self[R]], authentication.payload)
          case None => IO.raiseError(Authentication.Error.Forbidden)
        }
        .attemptNarrow[Authentication.Error]
  )

  def findUser(session: UUID): IO[Option[User]] =
    library.findBySession(Librarian.Session.fromUUID(session)).map(_.map(User.apply))
