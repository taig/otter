package io.taig.otter.sample.routes

import cats.Eval
import cats.effect.IO
import io.circe.Json
import io.taig.otter.http.{App, Routes}
import io.taig.otter.openapi.OpenApi
import io.taig.otter.sample.SampleRoute
import io.taig.otter.sample.api.Route
import io.taig.otter.sample.api.endpoints

final class OpenApiRoutes(route: SampleRoute, routes: Routes[IO]):
  val get: Route[Unit, Json] = route(endpoints.openapi.get): (_, _) =>
    IO.pure(Json.fromJsonObject(OpenApi(routes)))

object OpenApiRoutes:
  def apply(route: SampleRoute, routes: Routes[IO]): Routes[IO] =
    Routes(new OpenApiRoutes(route, routes).get)
