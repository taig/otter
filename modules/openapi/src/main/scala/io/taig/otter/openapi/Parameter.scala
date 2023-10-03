package io.taig.otter.openapi

final case class Parameter(
    in: "query" | "header" | "path" | "cookie",
    name: String,
    description: Option[String] = None,
    required: Boolean = false,
    deprecated: Boolean = false,
    schema: Option[Extended[Schema]] = None
)
