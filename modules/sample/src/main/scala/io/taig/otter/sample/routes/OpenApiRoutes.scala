package io.taig.otter.sample.routes

import cats.data.Chain
import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import io.taig.otter.Data
import io.taig.otter.http.Routes
import io.taig.otter.openapi.*
import io.taig.otter.openapi.circe.instance.given
import io.taig.otter.http.openapi.toOpenApi
import io.taig.otter.sample.Build
import io.taig.otter.sample.api.{endpoints, AuthenticatedRoute}
import io.taig.otter.sample.service.EndpointImplementation

final class OpenApiRoutes(implementation: EndpointImplementation, routes: Routes[IO]):
  val get: AuthenticatedRoute[Unit, Json] = implementation(endpoints.openapi.get): (_, _) =>
    val openapi = toOpenApi(
      routes.toChain.map(_.endpoint),
      title = "Otter Sample Library 🦦",
      description = "A simple library REST API that aims to showcase and test all features of the Otter library.".some,
      version = Build.version,
      servers = Chain(Server(url = "http://localhost:8080")),
      tags = Chain(
        Tag(name = "books"),
        Tag(name = "librarians", description = "Administrative accounts for library employees".some),
        Tag(name = "members", description = "Accounts for library members used to borrow and return books".some)
      ),
      securitySchemes = Chain(
        "Librarian" -> Data.Object.of(
          "type" -> Data.String("http"),
          "scheme" -> Data.String("bearer")
        ),
        "Member" -> Data.Object.of(
          "type" -> Data.String("http"),
          "scheme" -> Data.String("bearer")
        )
      )
    )

    IO.pure(openapi.asJson)

object OpenApiRoutes:
  def apply(implementation: EndpointImplementation, routes: Routes[IO]): Routes[IO] =
    Routes(new OpenApiRoutes(implementation, routes).get)
