package io.taig.otter.sample.api.endpoints

import io.circe.{Json, Printer}
import io.taig.otter.dsl.*
import io.taig.otter.http.Url
import io.taig.otter.sample.api.Role

object openapi:
  val url: Url[Unit] = __ / "openapi.json"

  val get: Endpoint[Role.Guest, Unit, Json] = Endpoint(
    request(method.get, url),
    response(result(code.ok, output.json(Printer.spaces2)))
  )
