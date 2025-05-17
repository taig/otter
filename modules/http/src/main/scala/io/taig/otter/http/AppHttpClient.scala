package io.taig.otter.http
import cats.ApplicativeThrow
import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.http.Headers.Data.accept
import io.taig.otter.http.HttpError.*

object AppHttpClient:
  def apply[F[_]: ApplicativeThrow, S[_], T[_], U[_]](
      decoder: PayloadDecoder[S + T + U],
      encoder: PayloadEncoder[S + T + U],
      debug: Boolean
  )(app: App[F, S, T, U]): HttpClient[F] = ???
  // new HttpClient[F]:
  //   override def submit[A, B](request: Request.Data): F[Response.Data] = app.routes
  //     .find(route => RequestMatcher(request = route.endpoint.request, data = request))
  //     .map: route =>
  //       RequestDataDecoder(decoder)(request = route.endpoint.request, data = request)
  //         .leftWiden[Failure | MediaTypeUnsupported | ValidationViolations]
  //         .flatTraverse(route.implementation(_).attempt.map(_.leftMap(Failure.apply)))
  //         .map(ResponseDataEncoder(encoder, debug)(response = route.endpoint.response, headers = request.headers, _))
  //     .getOrElse:
  //       ResultDataEncoder[U](encoder)(
  //         result = app.notFound,
  //         accept = request.headers.accept.getOrElse(none),
  //         ()
  //       ).getOrElse(ResultDataEncoder[U](encoder)(result = app.notFound, ())).pure[F]
