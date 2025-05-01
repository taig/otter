package io.taig.otter.http

import cats.Monad
import cats.syntax.all.*
import io.taig.otter.http.header.MediaType
import org.http4s.HttpRoutes as Http4sRoutes
import org.http4s.HttpApp as Http4sApp
import cats.MonadThrow
import cats.data.OptionT

def toHttp4sRoutes[F[_]: MonadThrow, S[_], T[_], U[_]](
    routes: Routes[F, S, T, U],
    encoder: BodyEncoder[[a] =>> S[a] | T[a] | U[a]]
): Http4sRoutes[F] = Http4sRoutes: request =>
  OptionT
    .liftF:
      Http4sMethodDecoder(method = request.method)
        .leftMap(violations => new IllegalStateException("Illegal method: " + violations))
        .liftTo[F]
    .map: method =>
      println("Find matching route")
      routes.toChain.find: route =>
        if route.endpoint.request.method === method
        then Http4sUrlMatcher(reference = route.endpoint.request.url, actual = request.uri)
        else false
    .flatMap:
      case Some(route) =>
        println("Run the endpoint")
        route.implementation(???)
        ???
      case None =>
        println("No match found")
        OptionT.none
    .onError: throwable =>
      throwable.printStackTrace()
      MonadThrow[OptionT[F, *]].raiseError(throwable)

def toHttp4sApp[F[_]: MonadThrow, S[_], T[_], U[_]](
    app: App[F, S, T, U],
    encoder: BodyEncoder[[a] =>> S[a] | T[a] | U[a]]
): Http4sApp[F] =
  val routes = toHttp4sRoutes(app.routes, encoder)

  Http4sApp: request =>
    // encode(???, app.error)
    routes(request).getOrElse(???)
