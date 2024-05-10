package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter.Collection
import io.taig.otter.Encoder
import io.taig.otter.Primitive
import io.taig.otter.Tuple

object JsonEncoder extends Encoder[JsonSchema.Writer, Json]:
  override def apply[A](schema: JsonSchema.Writer[A], a: A): Json = schema.unfix match
    case schema: Collection.Writer[JsonSchema.Writer[?], A] =>
      JsonCollectionEncoder(schema, a).map(values => Json.fromValues(values.toVector)).getOrElse(Json.Null)
    case schema: Primitive.Writer[A] => JsonPrimitiveEncoder(schema, a)
    case schema: Tuple.Writer[JsonSchema.Writer[?], A] =>
      JsonTupleEncoder(schema, a).map(values => Json.fromValues(values.toVector)).getOrElse(Json.Null)
