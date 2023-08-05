package io.taig.otter.http

final case class Endpoint[I, O](request: Request[I], response: Response[O]):
  def modifyRequest[T](f: Request[I] => Request[T]): Endpoint[T, O] = copy(request = f(request))
  def modifyResponse[T](f: Response[O] => Response[T]): Endpoint[I, T] = copy(response = f(response))

object Endpoint:
  final case class Implementation[F[_], I, O](endpoint: Endpoint[I, O], implementation: I => F[O]):
    def :+(endpoint: Endpoint.Implementation[F, ?, ?]): Routes[F] = toRoutes :+ endpoint
    def +:(endpoint: Endpoint.Implementation[F, ?, ?]): Routes[F] = endpoint +: toRoutes
    def toRoutes: Routes[F] = Routes.one(this)
