package io.taig.otter.openapi

final case class Header(
    name: String,
    description: Option[String] = None,
    externalDocs: Option[Extended[ExternalDocumentation]] = None
)
