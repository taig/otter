package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.+
import org.http4s.HttpRoutes as Http4sRoutes
import org.http4s.HttpApp as Http4sApp
import cats.effect.Concurrent
import cats.MonadThrow
import cats.data.OptionT
import org.typelevel.ci.*
import io.taig.otter.http.header.Accept
import io.taig.otter.Violations
import io.taig.otter.Step
import io.taig.otter.Violation

def toHttp4sRoutes[F[_]: Concurrent, S[_], T[_], U[_]](
    routes: Routes[F, S, T, U],
    decoder: PayloadDecoder[S],
    encoder: PayloadEncoder[S + T + U],
    debug: Boolean = false
): Http4sRoutes[F] = Http4sRoutes: request =>
  OptionT
    .liftF:
      Http4sMethodDecoder(method = request.method)
        .leftMap(violations => new IllegalStateException("Illegal method: " + violations))
        .liftTo[F]
    .subflatMap: method =>
      routes.toChain.find: route =>
        if route.endpoint.request.method === method
        then Http4sUrlMatcher(reference = route.endpoint.request.url, actual = request.uri)
        else false
    .semiflatMap: route =>
      Http4sRequestDecoder(decoder)(request = route.endpoint.request, value = request)
        .flatMap(_.traverse(route.implementation))
        .attempt
        .flatMap: value =>
          val accept = request.headers
            .get(ci"Accept")
            .map(_.head.value)
            .traverse: value =>
              Accept
                .parse(value)
                .leftMap: error =>
                  Violations.of((Step.Field("headers"), Violation.tpe(name = "Accept", actual = value)))
                .leftMap(Response.Error.ValidationViolations.apply)
            .toValidated

          Http4sResponseEncoder(encoder, debug)(
            response = route.endpoint.response,
            accept = accept.getOrElse(none),
            result = accept.fold(_.invalid.asRight, _ => value)
          )
    .onError: throwable =>
      throwable.printStackTrace()
      MonadThrow[OptionT[F, *]].raiseError(throwable)

def toHttp4sApp[F[_]: Concurrent, S[_], T[_], U[_]](
    app: App[F, S, T, U],
    encoder: PayloadEncoder[S + T + U],
    debug: Boolean = false
): Http4sApp[F] =
  val routes = toHttp4sRoutes(app.routes, decoder = ???, encoder, debug)

  Http4sApp: request =>
    // encode(???, app.error)
    routes(request).getOrElse(???)
