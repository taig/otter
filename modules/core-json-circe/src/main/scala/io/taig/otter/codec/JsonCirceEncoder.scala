package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json

object JsonCirceEncoder extends Encoder[Json.Write, CirceJson]:
  override def encode[A](json: Json.Write[A], a: A): CirceJson = json match
    case json: Json.Coerce.Write[A]     => CoerceEncoder(encoder = this).encode(schema = json.self.self, a)
    case json: Json.Constant.Write[A]   => ConstantEncoder(encoder = this).encode(schema = json.self.self, a)
    case json: Json.Collection.Write[A] =>
      CirceJson.fromValues(CollectionEncoder(encoder = this).encode(schema = json.self.self, a))
    case json: Json.Dictionary.Write[A] =>
      CirceJson.fromFields(DictionaryEncoder(encoder = this).encode(schema = json.self.self, a))
    case json: Json.Enumeration.Write[A] => EnumerationEncoder(encoder = this).encode(schema = json.self.self, a)
    case json: Json.Primitive.Write[A]   => JsonPrimitiveCirceEncoder.encode(json, a)
    case json: Json.Record.Write[A]      =>
      CirceJson.fromFields(RecordEncoder(encoder = JsonFieldCirceEncoder).encode(schema = json.self.self, a).toList)
    case json: Json.Optional.Write[A] =>
      OptionalEncoder(encoder = this, empty = CirceJson.Null).encode(schema = json.self.self, a)
    case json: Json.Tuple.Write[A] =>
      CirceJson.fromValues(TupleEncoder(encoder = this, empty = CirceJson.Null).encode(schema = json.self.self, a))
    case json: Json.Union.Write[A] =>
      UnionEncoder(encoder = JsonBranchCirceEncoder).encode(schema = json.self.self, a)
