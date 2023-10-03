package io.taig.otter.openapi

import cats.data.Chain

final case class RequestBody(
    content: Chain[(String, MediaType)] = Chain.empty,
    description: Option[String] = None,
    required: Boolean = false
)
