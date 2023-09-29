package io.taig.otter.http

final class RoutesClient[F[_]](routes: Routes[F]) extends Client[F]:
  override def submitRaw[I, O](endpoint: Endpoint[I, O], request: Http.Request): F[Http.Response] =
    endpoint.request.decode(request)
    ???
//    routes
//    .find(endpoint.request.method, endpoint.request.url)