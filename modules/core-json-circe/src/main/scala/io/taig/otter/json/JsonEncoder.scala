package io.taig.otter.json

import io.circe.Json
import io.taig.otter.*

object JsonEncoder extends Encoder[Codec[?, *], Json]:
  override def apply[A](schema: Codec[?, A], a: A): Json = schema match
    case schema: Collection[?, A]  => CollectionJsonEncoder(schema, a).fold(Json.Null)(Json.fromValues)
    case schema: Dictionary[?, A]  => DictionaryJsonEncoder(schema, a).fold(Json.Null)(Json.fromFields)
    case schema: Enumeration[?, A] => EnumerationJsonEncoder(schema, a)
    case schema: Primitive[A]      => PrimitiveJsonEncoder(schema, a)
    case schema: Product[?, A]     => ProductJsonEncoder(schema, a).fold(Json.Null)(Json.fromValues)
    case schema: Record[?, A] =>
      RecordJsonEncoder(schema, a).map(_.toList).fold(Json.Null)(Json.fromFields)
    case schema: Sum[?, A]   => SumJsonEncoder(schema, a).fold(Json.Null)(Json.fromJsonObject)
    case schema: Union[?, A] => UnionJsonEncoder(schema, a)
    case schema: Dynamic[A]  => DynamicJsonEncoder(schema, a)
