package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter.Tuple
import cats.Id as Identity
import io.taig.otter.Encoder
import io.taig.otter.Schema

object JsonEncoder extends Encoder[Schema.Writer[Identity, ?, *], Json]:
  def apply[A](schema: Schema.Writer[Identity, ?, A], a: A): Json = schema match
    case schema: Tuple.Writer[Identity, ?, A] => JsonTupleEncoder(schema, a).fold(Json.Null)(Json.fromValues)
