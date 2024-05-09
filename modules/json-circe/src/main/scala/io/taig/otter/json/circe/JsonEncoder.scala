package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter.Schema
import io.taig.otter.Collection
import io.taig.otter.Encoder

object JsonEncoder extends Encoder[[a] =>> Schema.Writer.Any[Schema.Writer.Identity[a], a], Json]:
  override def apply[B](schema: Schema.Writer.Any[Schema.Writer.Identity[B], B], b: B): Json = schema match
    case schema: Collection.Writer[Schema.Writer.Identity[B], B] =>
      JsonCollectionEncoder(schema, b).map(values => Json.fromValues(values.toVector)).getOrElse(Json.Null)
