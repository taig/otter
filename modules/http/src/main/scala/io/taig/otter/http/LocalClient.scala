package io.taig.otter.http

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.http.header.MediaType
import cats.syntax.all.*
import cats.Functor

final class LocalClient[F[_]: Functor, S[_], T[_], U[_]](routes: Routes[F, S, T, U]) extends Client[F, S, T, U]:
  val matcher = RequestMatcher(matcher = UrlMatcher.apply)

  override def submit[A, B](
      endpoint: Endpoint[S, T, U, A, B],
      contentType: Option[MediaType],
      a: A
  ): F[Validated[Violations, B]] =
    val route = routes.find(route => matcher(route, method = endpoint.request.method, url = endpoint.request.url))

    val bytes = HttpRequestEncoder[S].apply(endpoint.request, a)

    route.map: route =>
      HttpRequestDecoder[S]
        .apply(request = route.endpoint.request, bytes)
        .andThen: a =>
          route
            .implementation(a)
            .map: b =>
              val bytes = HttpResponseEncoder[T, U].apply(route.endpoint.response, b)
              HttpResponseDecoder[T, U].apply(endpoint.response, bytes)
          ???

    ???

object LocalClient:
  def apply[F[_]: Functor, S[_], T[_], U[_]](routes: Routes[F, S, T, U]): Client[F, S, T, U] =
    new LocalClient(routes)
