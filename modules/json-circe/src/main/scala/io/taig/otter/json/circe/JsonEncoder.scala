package io.taig.otter.json.circe

import io.circe.Json
import cats.Id as Identity
import io.taig.otter.Schema.*
import io.taig.otter.Plain.*
import io.taig.otter.Writer
import io.taig.otter.Encoder
import io.taig.otter as Base
import io.taig.otter.Plain
import io.taig.otter.Isomorphic

object JsonEncoder extends Encoder[Schema.Writer, Json]:
  override def apply[A](schema: Schema.Writer[A], a: A): Json = schema match
    case Writer.Root(schema)           => apply0(schema, a)
    case Isomorphic.Root(schema)       => apply0(schema, a)
    case Writer.Modify(self, f)        => apply(self, f(a))
    case Isomorphic.Modify(self, _, f) => apply(self, f(a))

  def apply0[A](
      schema: Base.Schema[
        Base.Data[Base.Writer[Base.Schema[Base.Data[Identity, Base.Writer, ?, *], *], *], Base.Writer, ?, *],
        A
      ],
      a: A
  ): Json = schema match
    case Base.Schema.Optional(self)      => a.map(apply0(self, _)).getOrElse(Json.Null)
    case Base.Schema.Required.Root(data) => apply0(data, a)
    case Base.Schema.Root(data)          => apply0(data, a)

  def apply0[A](
      data: Base.Data[
        Base.Writer[Base.Schema[Base.Data[Identity, Base.Writer, ?, *], *], *],
        Base.Writer,
        ?,
        A
      ],
      a: A
  ): Json = data match
    case schema: Base.Primitive[A] => JsonPrimitiveEncoder(schema, a)
    case schema: Base.Collection[Base.Writer[Base.Schema[Base.Data[Identity, Base.Writer, ?, *], *], *], ?, ?, A] =>
      Json.fromValues(JsonCollectionEncoder(schema, a))
