package io.taig.otter.sample.routes

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.http.Routes
import io.taig.otter.openapi.OpenApi
import io.taig.otter.sample.SampleRoute
import io.taig.otter.sample.api.Route
import io.taig.otter.sample.api.endpoints
import io.taig.otter.sample.Build

final class OpenApiRoutes(route: SampleRoute, routes: Routes[IO]):
  val get: Route[Unit, Json] = route(endpoints.openapi.get): (_, _) =>
    val openapi = OpenApi(
      title = "Otter Sample Library 🦦",
      description = "A simple library REST API that aims to showcase and test all features of the Otter library.".some,
      version = Build.version,
      routes
    )

    IO.pure(Json.fromJsonObject(openapi))

object OpenApiRoutes:
  def apply(route: SampleRoute, routes: Routes[IO]): Routes[IO] =
    Routes(new OpenApiRoutes(route, routes).get)
