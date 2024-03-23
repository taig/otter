// package io.taig.otter.openapi

// import cats.data.Chain
// import io.taig.otter.Data

// final case class Operation(
//     tags: Chain[String] = Chain.empty,
//     summary: Option[String] = None,
//     description: Option[String] = None,
//     externalDocs: Option[ExternalDocumentation] = None,
//     operationId: Option[String] = None,
//     parameters: Chain[Extended[Parameter] | Reference] = Chain.empty,
//     requestBody: Option[Extended[RequestBody] | Reference] = None,
//     responses: Extended[Responses] = Responses(),
//     callbacks: Map[String, Extended[Data.Object] | Reference] = Map.empty,
//     deprecated: Boolean = false,
//     security: Option[SecurityRequirement] = None,
//     servers: Chain[Extended[Server]] = Chain.empty
// )
