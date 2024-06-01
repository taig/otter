package io.taig.otter.json.circe

import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.taig.otter.Collection
import io.taig.otter.validation.Violations

// object JsonCollectionDecoder:
//   def apply[A](
//       schema: Base.Collection[Base.Reader[AsSchema, Base.Optional, Base.Schema, ?, ?], A],
//       values: Vector[Json]
//   ): Validated[Violations[Json, Json], A] = schema match
//     case Base.Collection.Root(schema) =>
//       values.zipWithIndex.traverse { case (json, index) =>
//         JsonDecoder(schema, json).leftMap(index /: _)
//       }
