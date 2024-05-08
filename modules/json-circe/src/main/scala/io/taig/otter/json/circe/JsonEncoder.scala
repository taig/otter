package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter.Schema
import io.taig.otter.Collection
import io.taig.otter.Encoder
import io.taig.otter.Fix
import io.taig.otter.Schema.Write

object JsonEncoder extends Encoder[[a] =>> Schema.Write[Schema.Write.Identity[a], a], Json]:
  override def apply[B](schema: Schema.Write[Schema.Write.Identity[B], B], b: B): Json = schema match
    case schema: Collection.Write[Schema.Write.Identity[B], B] =>
      JsonCollectionEncoder(schema, b).map(values => Json.fromValues(values.toVector)).getOrElse(Json.Null)
