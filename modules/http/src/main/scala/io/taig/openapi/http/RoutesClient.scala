package io.taig.openapi.http

import cats.MonadThrow
import cats.data.{NonEmptyChain, Validated}
import cats.effect.Concurrent
import cats.syntax.all.*

final class RoutesClient[F[+_]](routes: Routes[F])(using F: Concurrent[F]) extends Client[F]:
  override def submitRaw[I, O](endpoint: Endpoint[I, O], request: Request[F]): F[Response] = routes
    .find(request)
    .traverse { case Endpoint.Implementation(endpoint, implementation) =>
      endpoint.input.decode(request).flatMap(_.traverse(implementation.apply).map(endpoint.output.encode))
    }
    .flatMap(_.liftTo[F](new IllegalArgumentException("No route for request")))

object RoutesClient:
  def apply[F[+_]: Concurrent](routes: Routes[F]): Client[F] = new RoutesClient[F](routes)
