package io.taig.otter.http

import cats.syntax.all.*
import cats.ApplicativeThrow

final case class App[F[_]](routes: Routes[F], notFound: Response[Unit], failure: Response[Unit]):
  def apply(request: Http.Request[F], onError: Throwable => F[Unit])(using F: ApplicativeThrow[F]): F[Http.Response] =
    routes.find(request.method, request.url) match
      case Some(route) =>
        route
          .apply(request)
          .handleErrorWith: throwable =>
            onError(throwable) *> failure.encode(Request.Result.Success(())).pure[F]
      case None => notFound.encode(Request.Result.Success(())).pure[F]
