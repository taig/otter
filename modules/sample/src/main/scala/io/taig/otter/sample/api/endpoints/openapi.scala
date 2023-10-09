package io.taig.otter.sample.api.endpoints

import io.circe.{Json, Printer}
import io.taig.otter.dsl.*
import io.taig.otter.sample.api.Role

object openapi:
  val url: Url[Unit] = __ / "openapi.json"

  val get: AuthenticatedEndpoint[Role.Guest, Unit, Json] = endpoint(
    request(method.get, url),
    response(result(code.ok, output.json(json, Printer.spaces2)))
  ).role(Role.Guest)
