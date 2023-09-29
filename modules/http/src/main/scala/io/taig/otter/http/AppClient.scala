package io.taig.otter.http

import cats.Applicative
import cats.syntax.all.*

final class AppClient[F[_]: Applicative](app: App[F]) extends Client[F]:
  override def submitRaw(request: Http.Request): F[Http.Response] = app.routes
    .find(request.method, request.url)
    .traverse { route =>
      route.endpoint.request
        .decode(request)
        .traverse(route.implementation.apply)
        .map(route.endpoint.response.encode)
    }
    .map(_.getOrElse(app.notFound.encode(().valid)))

object AppClient:
  def apply[F[_]: Applicative](app: App[F]): Client[F] = new AppClient[F](app)
