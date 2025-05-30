package io.taig.otter.http

import cats.ApplicativeThrow
import cats.syntax.all.*
import io.taig.otter.http.Headers.Data.accept
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.codec.PayloadDecoder
import io.taig.otter.http.codec.PayloadEncoder
import io.taig.otter.http.codec.RequestDataDecoder
import io.taig.otter.http.codec.ResponseDataEncoder
import io.taig.otter.http.codec.ResultDataEncoder
import io.taig.otter.StacktracePrinter

object AppHttpClient:
  def apply[F[_]: ApplicativeThrow, S[_]](
      decoder: PayloadDecoder[S],
      encoder: PayloadEncoder[S],
      debug: Boolean
  )(app: App[F, S]): HttpClient[F] = new HttpClient[F]:
    override def submit[A, B](request: Request.Data): F[Response.Data] = app.routes
      .find(route => RequestMatcher(request = route.endpoint.request, data = request))
      .map: route =>
        RequestDataDecoder(decoder)
          .decode(schema = route.endpoint.request, value = request)
          .leftWiden[Failure | MediaTypeUnsupported | ValidationViolations]
          .flatTraverse: b =>
            route
              .implementation(b)
              .attempt
              .map(_.leftMap(throwable => Failure(stacktrace = StacktracePrinter(throwable).some)))
          .map: b =>
            ResponseDataEncoder(encoder, debug)
              .encode(schema = route.endpoint.response, headers = request.headers, b)
      .getOrElse:
        ResultDataEncoder(encoder)
          .encode(
            schema = app.notFound,
            accept = request.headers.accept.getOrElse(none),
            ()
          )
          .getOrElse(ResultDataEncoder(encoder).encode(schema = app.notFound, ()))
          .pure[F]
