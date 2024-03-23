// package io.taig.otter.openapi

// import cats.data.Chain
// import io.taig.otter.Data

// final case class PathItem(
//     ref: Option[String] = None,
//     summary: Option[String] = None,
//     description: Option[String] = None,
//     get: Option[Extended[Operation]] = None,
//     put: Option[Extended[Operation]] = None,
//     post: Option[Extended[Operation]] = None,
//     delete: Option[Extended[Operation]] = None,
//     options: Option[Extended[Operation]] = None,
//     head: Option[Extended[Operation]] = None,
//     patch: Option[Extended[Operation]] = None,
//     trace: Option[Extended[Operation]] = None,
//     servers: Chain[Extended[Server]] = Chain.empty,
//     parameters: Chain[Extended[Data.Object] | Reference] = Chain.empty
// )
