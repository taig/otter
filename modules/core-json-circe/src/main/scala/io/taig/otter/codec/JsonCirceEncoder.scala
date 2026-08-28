package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json

object JsonCirceEncoder extends Encoder[Json.Of, CirceJson]:
  override def encode[W](json: Json.Of[W, Any], w: W): CirceJson = (json: @unchecked) match
    case json: Json.Coerce.Of[W, ?]     => CoerceEncoder(JsonPrimitiveCirceEncoder).encode(json.self.self, w)
    case json: Json.Collection.Of[W, ?] =>
      CirceJson.fromValues(CollectionEncoder(this).encode(json.self.self, w))
    case json: Json.Constant.Of[W, ?]   => ConstantEncoder(JsonPrimitiveCirceEncoder).encode(json.self.self, w)
    case json: Json.Dictionary.Of[W, ?] =>
      CirceJson.fromFields(DictionaryEncoder(this).encode(json.self.self, w))
    case json: Json.Enumeration.Of[W, ?] =>
      EnumerationEncoder(JsonPrimitiveCirceEncoder).encode(json.self.self, w)
    case json: Json.Optional.Of[W, ?] =>
      OptionalEncoder(this, empty = CirceJson.Null).encode(json.self.self, w)
    case json: Json.Primitive.Of[W, ?] => JsonPrimitiveCirceEncoder.encode(json, w)
    case json: Json.Record.Of[W, ?]    =>
      CirceJson.fromFields(RecordEncoder(JsonFieldCirceEncoder).encode(json.self.self, w).toList)
    case json: Json.Tuple.Of[W, ?] =>
      CirceJson.fromValues(TupleEncoder(this, empty = CirceJson.Null).encode(json.self.self, w))
    case json: Json.Union.Of[W, ?] => UnionEncoder(JsonBranchCirceEncoder).encode(json.self.self, w)
