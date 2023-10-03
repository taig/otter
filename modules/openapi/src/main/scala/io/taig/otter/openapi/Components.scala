package io.taig.otter.openapi

import cats.data.Chain
import io.taig.otter.Data

final case class Components(
    schemas: Chain[(String, Schema)] = Chain.empty,
    responses: Chain[(String, Extended[Response] | Reference)] = Chain.empty,
    parameters: Chain[(String, Extended[Parameter] | Reference)] = Chain.empty,
    examples: Chain[(String, Extended[Data.Object] | Reference)] = Chain.empty,
    requestBodies: Chain[(String, Extended[RequestBody] | Reference)] = Chain.empty,
    headers: Chain[(String, Extended[Header] | Reference)] = Chain.empty,
    securitySchemes: Chain[(String, Extended[Data.Object] | Reference)] = Chain.empty,
    links: Chain[(String, Extended[Data.Object] | Reference)] = Chain.empty,
    callbacks: Chain[(String, Extended[Data.Object] | Reference)] = Chain.empty,
    pathItems: Chain[(String, Extended[PathItem] | Reference)] = Chain.empty
)
