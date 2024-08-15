package io.taig.otter.http

import cats.Monad
import cats.syntax.all.*

final case class Route[F[_], I, O](endpoint: Endpoint[I, O], implementation: I => F[O]):
  def apply(request: Http.Request)(using Monad[F]): F[Http.Response] = endpoint.request
    .decode(request)
    .flatMap(_.traverse(implementation))
    .map(endpoint.response.encode)

  def :+(endpoint: Route[F, ?, ?]): Routes[F] = toRoutes :+ endpoint

  def +:(endpoint: Route[F, ?, ?]): Routes[F] = endpoint +: toRoutes

  def toRoutes: Routes[F] = Routes.one(this)
