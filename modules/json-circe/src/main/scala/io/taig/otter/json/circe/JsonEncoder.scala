package io.taig.otter.json.circe

import io.circe.Json
import cats.Id as Identity
import io.taig.otter.Encoder
import io.taig.otter.Plain.*

object JsonEncoder extends Encoder[Schema.Writer, Json]:
  def apply[A](schema: Schema.Writer[A], a: A): Json = schema match
    case schema: Primitive.Writer[A] => JsonPrimitiveEncoder(schema, a)
    case schema: Tuple.Writer[A]     => JsonTupleEncoder(schema, a).fold(Json.Null)(Json.fromValues)
