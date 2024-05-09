package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter.Schema
import io.taig.otter.Collection
import io.taig.otter.Encoder
import io.taig.otter.Primitive

object JsonEncoder extends Encoder[[a] =>> Schema.Writer.Any[Schema.Writer.Identity[a], a], Json]:
  override def apply[A](schema: Schema.Writer.Any[Schema.Writer.Identity[A], A], a: A): Json = schema match
    case schema: Collection.Writer[Schema.Writer.Identity[A], A] =>
      JsonCollectionEncoder(schema, a).map(values => Json.fromValues(values.toVector)).getOrElse(Json.Null)
    case schema: Primitive.Writer[A] => JsonPrimitiveEncoder(schema, a)
