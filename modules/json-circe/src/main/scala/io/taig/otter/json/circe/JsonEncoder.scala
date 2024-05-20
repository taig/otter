package io.taig.otter.json.circe

import io.circe.Json
import io.taig.otter.Schema.*
import io.taig.otter.Plain.*
import io.taig.otter.Encoder
import io.taig.otter as Base
import io.taig.otter.Data
import cats.Id as Identity

object JsonEncoder extends Encoder[Schema.Writer, Json]:
  override def apply[A](schema: Schema.Writer[A], a: A): Json = schema match
    case Required.Writer.Root(data) => apply(data, a)
    case Required.Root(data)        => apply(data, a)
    case Optional(self)             => a.map(apply(self, _)).getOrElse(Json.Null)
    case Writer.Optional(self)      => a.map(apply(self, _)).getOrElse(Json.Null)

  def apply[A](data: Data[Identity, A], a: A): Json = data match
    case schema: Base.Primitive[A]            => JsonPrimitiveEncoder(schema, a)
    case schema: Base.Collection[Identity, A] => JsonCollectionEncoder(schema, a).fold(Json.Null)(Json.fromValues)
    case schema: Base.Tuple[Identity, A]      => Json.fromValues(JsonTupleEncoder(schema, a))
