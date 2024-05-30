package io.taig.otter.json.circe

import io.circe.Json
import cats.Id as Identity
import io.taig.otter.Plain.*
import io.taig.otter.Writer
import io.taig.otter.Encoder
import io.taig.otter as Base
import io.taig.otter.Plain
import io.taig.otter.Isomorphic

object JsonEncoder extends Encoder[Schema.Writer, Json]:
  override def apply[A](schema: Schema.Writer[A], a: A): Json = schema match
    case Writer.Root(schema)     => apply(schema, a)
    case Isomorphic.Root(schema) => apply(schema, a)
    // case Writer.Modify(self, f)        => apply(self, f(a))
    // case Isomorphic.Modify(self, _, f) => apply(self, f(a))

  def apply[A](
      schema: Base.Optional[Base.Schema[Base.WriterAny[AsSchema], *], A],
      a: A
  ): Json = schema match
    case Base.Optional.Root(self) => a.map(apply(self, _)).getOrElse(Json.Null)
    case Base.Required(data)      => apply(data, a)

  def apply[A](
      data: Base.Schema[Base.WriterAny[AsSchema], A],
      a: A
  ): Json = data match
    case schema: Base.Primitive[A] => JsonPrimitiveEncoder(schema, a)
    case schema: Base.Collection[Base.WriterAny[AsSchema], A] =>
      Json.fromValues(JsonCollectionEncoder(schema, a))
