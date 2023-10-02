package io.taig.otter.openapi

import cats.data.Chain
import io.taig.otter.Data

enum Schema:
  case Array(
      items: Schema,
      format: Option[String] = None,
      description: Option[String] = None
  )
  case Enumeration(
      tpe: String,
      enums: Chain[Data.Primitive]
  )
  case OneOf(schemas: Chain[Schema])
  case Object(
      format: Option[String] = None,
      description: Option[String] = None,
      properties: Chain[(String, Schema)] = Chain.empty
  )
  case Value(
      tpe: String,
      format: Option[String] = None,
      description: Option[String] = None
  )
