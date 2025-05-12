package io.taig.otter.http

import cats.MonadThrow
import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.Headers.Data.accept

final class LocalClient[F[_]: MonadThrow, S[_], T[_], U[_]](
    override val decoder: PayloadDecoder[S + T + U],
    override val encoder: PayloadEncoder[S + T + U],
    debug: Boolean
)(app: App[F, S, T, U])
    extends Client[F, S, T, U]:
  val writer = ResponseDataEncoder(encoder, debug)
  override def submit[A, B](request: Request.Data): F[Response.Data] =
    app.routes
      .find(route => RequestMatcher(request = route.endpoint.request, data = request))
      .map: route =>
        RequestDataDecoder(decoder)(request = route.endpoint.request, data = request)
          .leftWiden[Failure | MediaTypeUnsupported | ValidationViolations]
          .flatTraverse(route.implementation(_).attempt.map(_.leftMap(Failure.apply)))
          .map(writer(response = route.endpoint.response, headers = request.headers, _))
      .getOrElse(
        ResultDataEncoder[U](encoder)(
          result = app.notFound,
          accept = request.headers.accept.getOrElse(none),
          ()
        ).getOrElse(ResultDataEncoder[U](encoder)(result = app.notFound, ()))
          .pure[F]
      )

object LocalClient:
  def apply[F[_]: MonadThrow, S[_], T[_], U[_]](
      decoder: PayloadDecoder[S + T + U],
      encoder: PayloadEncoder[S + T + U],
      debug: Boolean
  )(app: App[F, S, T, U]): Client[F, S, T, U] = new LocalClient(decoder, encoder, debug)(app)
