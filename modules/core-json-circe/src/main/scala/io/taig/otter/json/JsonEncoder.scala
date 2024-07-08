package io.taig.otter.json

import io.circe.Json
import io.taig.otter as Base
import io.taig.otter.Encoder
import io.taig.otter.Plain.*

object JsonEncoder extends Encoder[Schema.Writer, Json]:
  override def apply[A](schema: Schema.Writer[A], a: A): Json = schema match
    case schema: Collection.Writer[A]  => CollectionJsonEncoder(schema, a).fold(Json.Null)(Json.fromValues)
    case schema: Dictionary.Writer[A]  => DictionaryJsonEncoder(schema, a).fold(Json.Null)(Json.fromFields)
    case schema: Enumeration.Writer[A] => EnumerationJsonEncoder(schema, a)
    case schema: Primitive.Writer[A]   => PrimitiveJsonEncoder(schema, a)
    case schema: Product.Writer[A]     => ProductJsonEncoder(schema, a).fold(Json.Null)(Json.fromValues)
    case schema: Record.Writer[A]      => RecordJsonEncoder(schema, a).map(_.toList).fold(Json.Null)(Json.fromFields)
    case schema: Sum.Writer[A]         => SumJsonEncoder(schema, a).fold(Json.Null)(Json.fromJsonObject)
    case schema: Union.Writer[A]       => UnionJsonEncoder(schema, a)
