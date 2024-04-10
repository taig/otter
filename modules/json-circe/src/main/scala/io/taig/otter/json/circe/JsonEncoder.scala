package io.taig.otter.json.circe

import io.taig.otter.Encoder
import io.circe.Json
import io.taig.otter.Primitive
import io.taig.otter.Tuple
import io.taig.otter.Schema
import io.taig.otter.Union

object JsonEncoder extends Encoder[Json]:
  override def encode[A](schema: Primitive[A], value: A): Json = JsonPrimitiveEncoder.encode(schema, value)

  override def encode[A](schema: Tuple[Schema[?], A], value: A): Json =
    JsonTupleEncoder(schema, value).map(values => Json.fromValues(values.toVector)).getOrElse(Json.Null)

  override def encode[A](schema: Union[Schema[?], A], value: A): Json = ???
