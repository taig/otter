package io.taig.otter.sample.routes

import cats.data.Chain
import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import io.taig.otter.http.Routes
import io.taig.otter.openapi.*
import io.taig.otter.sample.SampleRoute
import io.taig.otter.sample.api.Route
import io.taig.otter.sample.api.endpoints
import io.taig.otter.sample.Build

final class OpenApiRoutes(route: SampleRoute, routes: Routes[IO]):
  val get: Route[Unit, Json] = route(endpoints.openapi.get): (_, _) =>
    val openapi = toOpenApi(
      routes,
      title = "Otter Sample Library 🦦",
      description = "A simple library REST API that aims to showcase and test all features of the Otter library.".some,
      version = Build.version,
      servers = Chain(Server(url = "http://localhost:8080")),
      tags = Chain(
        Tag(name = "books"),
        Tag(name = "librarians", description = "Administrative accounts for library employees".some),
        Tag(name = "members", description = "Accounts for library members used to borrow and return books".some)
      ),
      securitySchemes = Json.obj(
        "Librarian" := Json.obj(
          "type" := "http",
          "scheme" := "bearer"
        ),
        "Member" := Json.obj(
          "type" := "http",
          "scheme" := "bearer"
        )
      )
    )

    IO.pure(openapi.asJson)

object OpenApiRoutes:
  def apply(route: SampleRoute, routes: Routes[IO]): Routes[IO] =
    Routes(new OpenApiRoutes(route, routes).get)
