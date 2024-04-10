package io.taig.otter.json.circe

import io.taig.otter.Encoder
import io.circe.Json
import io.taig.otter.Primitive
import io.taig.otter.Tuple

object JsonEncoder extends Encoder[Json]:
  override def encode[A](schema: Primitive[A], value: A): Json = JsonPrimitiveEncoder.encode(schema, value)

  override def encode[A](schema: Tuple[A], value: A): Json =
    JsonTupleEncoder(schema, value).map(values => Json.fromValues(values.toVector)).getOrElse(Json.Null)
