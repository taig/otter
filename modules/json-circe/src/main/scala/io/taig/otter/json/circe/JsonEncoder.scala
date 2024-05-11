package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter.Encoder
import io.taig.otter.Plain

object JsonEncoder extends Encoder[Plain.Schema.Writer[?, *], Json]:
  override def apply[A](schema: Plain.Schema.Writer[?, A], a: A): Json = schema match
    case schema: Plain.Primitive.Writer[A] => JsonPrimitiveEncoder(schema, a)
    case schema: Plain.Tuple.Writer[?, A] =>
      JsonTupleEncoder(schema, a).fold(Json.Null)(values => Json.fromValues(values.toVector))
