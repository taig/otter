package io.taig.otter.json.circe

import io.taig.otter as Base
import io.taig.otter.Plain.*
import io.circe.Json
import cats.syntax.all.*

// object JsonCollectionEncoder:
//   def apply[A](data: Base.Collection[Base.Writer[AsSchema, Base.Optional, Base.Schema, ?, ?], A], a: A): Vector[Json] =
//     data match
//       case Base.Collection.Root(schema) => a.map(JsonEncoder(schema, _))
