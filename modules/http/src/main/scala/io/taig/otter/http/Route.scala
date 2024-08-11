package io.taig.otter.http

final case class Route[F[_], I, O](endpoint: Endpoint[I, O], implementation: I => F[O]):
  def apply(request: Http.Request[F]): F[Http.Response] = ???
  //     payload
  //       .map(Http.Request(method, url, headers, _))
  //       .map(route.endpoint.request.decode)
  //       .flatMap(_.traverse(route.implementation))
  //       .map(route.endpoint.response.encode)
  //   .handleErrorWith: throwable =>
  //     onError(throwable).as(app.failure.encode(Request.Result.Success(())))
  // }

  def :+(endpoint: Route[F, ?, ?]): Routes[F] = toRoutes :+ endpoint

  def +:(endpoint: Route[F, ?, ?]): Routes[F] = endpoint +: toRoutes

  def toRoutes: Routes[F] = Routes.one(this)
