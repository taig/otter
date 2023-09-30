package io.taig.otter.sample.routes

import cats.data.Chain
import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import io.taig.otter.http.Routes
import io.taig.otter.openapi.OpenApi
import io.taig.otter.sample.SampleRoute
import io.taig.otter.sample.api.Route
import io.taig.otter.sample.api.endpoints
import io.taig.otter.sample.Build

final class OpenApiRoutes(route: SampleRoute, routes: Routes[IO]):
  val get: Route[Unit, Json] = route(endpoints.openapi.get): (_, _) =>
    val openapi = OpenApi(
      routes,
      title = "Otter Sample Library 🦦",
      description = "A simple library REST API that aims to showcase and test all features of the Otter library.".some,
      version = Build.version,
      tags = Chain(
        Json.obj(
          "name" := "books",
          "externalDocs" := Json.obj(
            "description" := "Lorem ipsum dolar sit amet",
            "url" := "https://www.hellobonnie.de/"
          )
        ),
        Json.obj(
          "name" := "librarians",
          "description" := "Administrative accounts for library employees"
        ),
        Json.obj(
          "name" := "members",
          "description" := "Accounts for library members used to borrow and return books"
        )
      )
    )

    IO.pure(openapi)

object OpenApiRoutes:
  def apply(route: SampleRoute, routes: Routes[IO]): Routes[IO] =
    Routes(new OpenApiRoutes(route, routes).get)
