package io.taig.otter.sample.service

import cats.effect.IO
import cats.syntax.all.*
import io.taig.otter.http.Route
import io.taig.otter.sample.User
import io.taig.otter.sample.api.{Role, Self}
import io.taig.otter.sample.data.Session
import io.taig.otter.sample.repository.LibrarianRepository

// final class EndpointImplementation(librarians: LibrarianRepository):
//   def apply[R <: Role, I, O](endpoint: AuthenticatedEndpoint[R, I, O])(
//       f: (Self[R], I) => IO[O]
//   ): AuthenticatedRoute[I, O] = Route(
//     endpoint.toAuthenticatedEndpoint,
//     authentication =>
//       authentication.session
//         .match {
//           case Some(session) =>
//             for
//               user <- findUser(session).flatMap(_.liftTo[IO](Authentication.Error.UserUnknown))
//               _ <- IO.raiseWhen(!endpoint.role.toSet.contains(user.role))(Authentication.Error.Forbidden)
//               response <- f(user.asInstanceOf[Self[R]], authentication.payload)
//             yield response
//           case None if endpoint.role.toSet.contains(Role.Guest) =>
//             f(().asInstanceOf[Self[R]], authentication.payload)
//           case None => IO.raiseError(Authentication.Error.Forbidden)
//         }
//         .attemptNarrow[Authentication.Error]
//   )

//   def findUser(session: Session): IO[Option[User]] = librarians.findBySession(session)
