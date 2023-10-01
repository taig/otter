package io.taig.otter.http

final case class Route[F[_], R, I, O](endpoint: Endpoint[R, I, O], implementation: I => F[O]):
  def :+(endpoint: Route[F, ?, ?, ?]): Routes[F] = toRoutes :+ endpoint
  def +:(endpoint: Route[F, ?, ?, ?]): Routes[F] = endpoint +: toRoutes
  def toRoutes: Routes[F] = Routes.one(this)
