package io.taig.otter.openapi

final case class Tag(
    name: String,
    description: Option[String] = None,
    externalDocs: Option[ExternalDocumentation] = None
)
