package io.taig.otter.http

import cats.MonadThrow
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.Step
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.http.header.Accept
import org.http4s.Request as Http4sRequest
import org.http4s.Headers as Http4sHeaders
import org.http4s.HttpApp as Http4sApp
import org.http4s.HttpRoutes as Http4sRoutes
import org.typelevel.ci.*
import org.http4s.Uri as Http4sUri
import cats.data.Chain

def toUrlData(uri: Http4sUri): Url.Data = Url.Data(
  path = Chain.fromSeq(uri.path.segments).map(_.encoded),
  queries = Chain.fromSeq(uri.query.toVector)
)

def toHeadersData(headers: Http4sHeaders): Headers.Data =
  Chain.fromSeq(headers.headers.map(header => (header.name, header.value)))

def toRequestData[F[_]: Concurrent](request: Http4sRequest[F]): F[Request.Data] =
  request.body.compile
    .to(Array)
    .map: body =>
      Request.Data(
        method = Method(request.method.name),
        url = toUrlData(uri = request.uri),
        headers = toHeadersData(headers = request.headers),
        body
      )

def toHttp4sRoutes[F[_]: Concurrent, S[_], T[_], U[_]](
    routes: Routes[F, S, T, U],
    decoder: PayloadDecoder[S],
    encoder: PayloadEncoder[S + T + U],
    debug: Boolean = false
): Http4sRoutes[F] = Http4sRoutes:
  request =>
    toRequestData(request).map:
      request =>
        routes
          .find(route => RequestMatcher(request = route.endpoint.request, data = request))
          .map: route =>
            RequestDataDecoder(decoder)(request = route.endpoint.request, data = request)
              .traverse(route.implementation)
              .attempt
              .flatMap: value =>
                ResponseEncoder[T, U].apply(response = route.endpoint.response, value)
                ???
            ???
        ???
        //   RequestDataDecoder[F].apply()
    ???
    // OptionT
    //   .liftF:
    //     Http4sMethodDecoder(method = request.method)
    //       .leftMap(violations => new IllegalStateException("Illegal method: " + violations))
    //       .liftTo[F]
    //   .subflatMap(method => routes.find(matcher(_, method, url = request.uri)))
    //   .semiflatMap: route =>
    //     Http4sRequestDecoder(decoder)(request = route.endpoint.request, value = request)
    //       .flatMap(_.traverse(route.implementation))
    //       .attempt
    //       .flatMap: value =>
    //         val accept = request.headers
    //           .get(ci"Accept")
    //           .map(_.head.value)
    //           .traverse: value =>
    //             Accept
    //               .parse(value)
    //               .leftMap: error =>
    //                 Violations.of((Step.Field("headers"), Violation.tpe(name = "Accept", actual = value)))
    //               .leftMap(Request.Error.ValidationViolations.apply)
    //           .toValidated

    //         Http4sResponseEncoder(encoder, debug)(
    //           response = route.endpoint.response,
    //           accept = accept.getOrElse(none),
    //           result = accept.fold(_ => ???, _ => value)
    //         )
    //   .onError: throwable =>
    //     throwable.printStackTrace()
    //     MonadThrow[OptionT[F, *]].raiseError(throwable)

def toHttp4sApp[F[_]: Concurrent, S[_], T[_], U[_]](
    routes: Routes[F, S, T, U],
    encoder: PayloadEncoder[S + T + U],
    debug: Boolean = false
): Http4sApp[F] =
  val http4s = toHttp4sRoutes(routes, decoder = ???, encoder, debug)

  Http4sApp: request =>
    // encode(???, app.error)
    http4s(request).getOrElse(???)
