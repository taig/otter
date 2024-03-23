// package io.taig.otter.openapi

// import cats.data.Chain

// final case class OpenApi(
//     openapi: String,
//     info: Extended[Info],
//     jsonSchemaDialect: Option[String] = None,
//     servers: Chain[Extended[Server]] = Chain.empty,
//     paths: Paths = Paths.Empty,
//     webhooks: Map[String, PathItem | Reference] = Map.empty,
//     components: Extended[Components] = Components(),
//     security: Option[SecurityRequirement] = None,
//     tags: Chain[Extended[Tag]] = Chain.empty,
//     externalDocs: Option[Extended[ExternalDocumentation]] = None
// )
