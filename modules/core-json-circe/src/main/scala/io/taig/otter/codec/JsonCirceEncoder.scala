package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json

object JsonCirceEncoder extends Encoder[Json, CirceJson]:
  override def encode[A](schema: Json[A], a: A): CirceJson = schema match
    case Json.Coerce(schema)     => CoerceEncoder(encoder = this).encode(schema = schema.self, a)
    case Json.Collection(schema) =>
      CirceJson.fromValues(CollectionEncoder(encoder = this).encode(schema = schema.self, a))
    case Json.Constant(schema)   => ConstantEncoder(encoder = this).encode(schema = schema.self, a)
    case Json.Dictionary(schema) =>
      CirceJson.fromFields(DictionaryEncoder(encoder = this).encode(schema = schema.self, a))
    case Json.Enumeration(schema) => EnumerationEncoder(encoder = this).encode(schema = schema.self, a)
    case Json.Nullable(schema)    =>
      NullableEncoder(encoder = this, empty = CirceJson.Null).encode(schema = schema.self, a)
    case schema: Json.Primitive[?] => JsonPrimitiveCirceEncoder.encode(schema, a)
    case Json.Record(schema)       =>
      CirceJson.fromFields(RecordEncoder(encoder = JsonFieldCirceEncoder).encode(schema = schema.self, a).toList)
    case Json.Tuple(schema) =>
      CirceJson.fromValues(TupleEncoder(encoder = this, empty = CirceJson.Null).encode(schema = schema.self, a))
    case Json.Union(schema) => UnionEncoder(encoder = this).encode(schema = schema.self, a)
