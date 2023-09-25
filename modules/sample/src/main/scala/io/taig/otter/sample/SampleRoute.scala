package io.taig.otter.sample

import cats.data.Chain
import cats.effect.IO
import cats.effect.std.AtomicCell
import io.taig.otter.http.Route as OtterRoute
import io.taig.otter.sample.api.endpoints.Endpoint
import io.taig.otter.sample.api.{Librarian, Member, Role, Route}

import java.util.UUID

final class SampleRoute(librarians: AtomicCell[IO, Chain[Librarian]]):
  def apply[R, I, O](endpoint: Endpoint[R, I, O])(f: (SampleRoute.Self[R], I) => IO[O]): Route[I, O] = OtterRoute(
    endpoint.toUnauthenticatedEndpoint,
    authentication =>
      authentication.self match
        case Some(session) =>
          val user: SampleRoute.Self[R] = ???
          ???
        case None => ???
  )

  def findUser(session: UUID): IO[Option[Librarian.Summary | Member]] = ???

object SampleRoute:
  type Self[R] = R match
    case Role.Guest     => Unit
    case Role.Member    => Member
    case Role.Librarian => Librarian
    case Either[a, b]   => Self[a] | Self[b]
