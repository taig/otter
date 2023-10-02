package io.taig.otter.openapi

import cats.data.NonEmptyMap

final case class RequestBody(
    content: NonEmptyMap[String, MediaType],
    description: Option[String] = None,
    required: Boolean = false
)
