package io.taig.otter.http

import cats.MonadThrow
import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.http.HttpError.*

final class LocalClient[F[_]: MonadThrow, S[_], T[_], U[_]](
    override val decoder: PayloadDecoder[S + T + U],
    override val encoder: PayloadEncoder[S + T + U],
    debug: Boolean
)(routes: Routes[F, S, T, U])
    extends Client[F, S, T, U]:
  override def submit[A, B](request: Request.Data): F[Response.Data] = routes
    .find(route => RequestMatcher(request = route.endpoint.request, data = request))
    .liftTo[F](new IllegalArgumentException("No route for request"))
    .flatMap: route =>
      RequestDataDecoder(decoder)(request = route.endpoint.request, data = request)
        .traverse(route.implementation)
        .handleError(Failure(_).asLeft)
        .map: result =>
          ResponseDataEncoder(encoder, debug)(response = route.endpoint.response, headers = request.headers, result)

object LocalClient:
  def apply[F[_]: MonadThrow, S[_], T[_], U[_]](
      decoder: PayloadDecoder[S + T + U],
      encoder: PayloadEncoder[S + T + U],
      debug: Boolean
  )(routes: Routes[F, S, T, U]): Client[F, S, T, U] = new LocalClient(decoder, encoder, debug)(routes)
