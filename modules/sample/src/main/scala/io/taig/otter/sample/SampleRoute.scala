package io.taig.otter.sample

import cats.effect.IO
import io.taig.otter.http.Route as OtterRoute
import io.taig.otter.sample.api.endpoints.Endpoint
import io.taig.otter.sample.api.{Role, Route}

final class SampleRoute:
  def apply[R, I, O](endpoint: Endpoint[R, I, O])(
      f: (SampleRoute.Self[R], I) => IO[O]
  ): Route[I, O] =
    val user: SampleRoute.Self[R] = ???
    // Route(endpoint.toUnauthenticatedEndpoint, f(user, _))
    OtterRoute(endpoint.toUnauthenticatedEndpoint, ???)

object SampleRoute:
  type Self[R] = R match
    case Role.Guest     => Unit
    case Role.Member    => Member
    case Role.Librarian => Librarian
    case Either[a, b]   => Self[a] | Self[b]
