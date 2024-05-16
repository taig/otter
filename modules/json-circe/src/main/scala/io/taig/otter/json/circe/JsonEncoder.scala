package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter as Base
import io.taig.otter.Plain.*
import cats.Id as Identity
import io.taig.otter.Encoder

object JsonEncoder extends Encoder[Schema, Json]:
  def apply[A](schema: Schema[A], a: A): Json = schema match
    case Base.Schema.Root(data) => apply(data, a)

  def apply[A](data: Base.Data[Identity, ?, A], a: A): Json = data match
    case data: Base.Primitive[A] => JsonPrimitiveEncoder(data, a)
    case data: Base.Tuple[Identity, ?, A] =>
      JsonTupleEncoder(data, a).fold(Json.Null)(values => Json.fromValues(values.toVector))
