package io.taig.otter.sample

import cats.syntax.all.*
import cats.effect.IO
import io.taig.otter.http.Route as OtterRoute
import io.taig.otter.sample.api.endpoints.Endpoint
import io.taig.otter.sample.api.endpoints.Authentication
import io.taig.otter.sample.api.{Role, Route, User}
import io.taig.otter.sample.data.{Librarian, Member}
import io.taig.otter.sample.repository.LibrarianRepository

import java.util.UUID

final class SampleRoute(library: LibrarianRepository):
  def apply[R, I, O](endpoint: Endpoint[R, I, O])(f: (SampleRoute.Self[R], I) => IO[O]): Route[I, O] = OtterRoute(
    endpoint.toUnauthenticatedEndpoint,
    authentication =>
      authentication.self
        .match {
          case Some(session) =>
            for
              user <- findUser(session).flatMap(_.liftTo[IO](Authentication.Error.UserUnknown))
              _ <- IO.raiseWhen(!endpoint.roles.contains(user.toRole))(Authentication.Error.Forbidden)
              response <- f(user.asInstanceOf[SampleRoute.Self[R]], authentication.payload)
            yield response
          case None if endpoint.roles.contains(Role.Guest) =>
            f(().asInstanceOf[SampleRoute.Self[R]], authentication.payload)
          case None => IO.raiseError(Authentication.Error.Forbidden)
        }
        .attemptNarrow[Authentication.Error]
  )

  def findUser(session: UUID): IO[Option[User]] =
    library.findBySession(Librarian.Session.fromUUID(session)).map(_.map(User.apply))

object SampleRoute:
  type Self[R] = R match
    case Role.Guest     => Unit
    case Role.Member    => Member
    case Role.Librarian => Librarian.Summary
    case Either[a, b]   => Self[a] | Self[b]
