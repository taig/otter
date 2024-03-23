// package io.taig.otter.openapi

// import cats.data.Chain
// import io.taig.otter.Data

// enum Schema:
//   case Array(
//       items: Schema | Reference,
//       format: Option[String] = None,
//       description: Option[String] = None
//   )
//   case Enumeration(
//       tpe: String,
//       enums: Chain[Data.Primitive]
//   )
//   case OneOf(codecs: Chain[Schema | Reference], discriminator: Option[Discriminator] = None)
//   case Object(
//       format: Option[String] = None,
//       description: Option[String] = None,
//       properties: Chain[(String, Schema | Reference)] = Chain.empty,
//       required: Chain[String] = Chain.empty
//   )
//   case Value(
//       tpe: String,
//       format: Option[String] = None,
//       description: Option[String] = None,
//       additionalProperties: Option[Schema | Reference] = None
//   )
