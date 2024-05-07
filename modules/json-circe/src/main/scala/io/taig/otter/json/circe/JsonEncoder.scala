package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter.Schema
import io.taig.otter.Collection
import io.taig.otter.Encoder

object JsonEncoder extends Encoder[Schema.Write.Any[Schema.Write.Any[?, ?], *], Json]:
  override def apply[A](schema: Schema.Write.Any[Schema.Write.Any[?, ?], A], a: A): Json = schema match
    case schema: Collection.Write[Schema.Write.Any[?, ?], A] =>
      JsonCollectionEncoder(schema, a).map(values => Json.fromValues(values.toVector)).getOrElse(Json.Null)
