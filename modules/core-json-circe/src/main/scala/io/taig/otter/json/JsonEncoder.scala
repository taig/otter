package io.taig.otter.json

import io.circe.Json
import io.taig.otter as Base
import io.taig.otter.Encoder
import io.taig.otter.Plain.*

object JsonEncoder extends Encoder[Schema.Writer.Via[Json, *], Json]:
  override def apply[A](schema: Schema.Writer.Via[Json, A], a: A): Json = schema match
    case schema: Collection.Writer.Via[Json, A]  => CollectionJsonEncoder(schema, a).fold(Json.Null)(Json.fromValues)
    case schema: Dictionary.Writer.Via[Json, A]  => DictionaryJsonEncoder(schema, a).fold(Json.Null)(Json.fromFields)
    case schema: Enumeration.Writer.Via[Json, A] => EnumerationJsonEncoder(schema, a)
    case schema: Primitive.Writer[A]             => PrimitiveJsonEncoder(schema, a)
    case schema: Product.Writer.Via[Json, A]     => ProductJsonEncoder(schema, a).fold(Json.Null)(Json.fromValues)
    case schema: Record.Writer.Via[Json, A] =>
      RecordJsonEncoder(schema, a).map(_.toList).fold(Json.Null)(Json.fromFields)
    case schema: Sum.Writer.Via[Json, A]   => SumJsonEncoder(schema, a).fold(Json.Null)(Json.fromJsonObject)
    case schema: Union.Writer.Via[Json, A] => UnionJsonEncoder(schema, a)
    case schema: Dynamic.Writer[?, A]      => DynamicJsonEncoder(???, a)
