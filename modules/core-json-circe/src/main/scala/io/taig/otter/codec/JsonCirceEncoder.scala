package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json

object JsonCirceEncoder extends Encoder[Json, CirceJson]:
  override def encode[W](json: Json[W, Any], w: W): CirceJson = (json: @unchecked) match
    case json: Json.Coerce[W, ?]     => CoerceEncoder(JsonPrimitiveCirceEncoder).encode(json.self.self, w)
    case json: Json.Collection[W, ?] =>
      CirceJson.fromValues(CollectionEncoder(this).encode(json.self.self, w))
    case json: Json.Constant[W, ?]   => ConstantEncoder(JsonPrimitiveCirceEncoder).encode(json.self.self, w)
    case json: Json.Dictionary[W, ?] =>
      CirceJson.fromFields(DictionaryEncoder(this).encode(json.self.self, w))
    case json: Json.Enumeration[W, ?] =>
      EnumerationEncoder(JsonPrimitiveCirceEncoder).encode(json.self.self, w)
    case json: Json.Optional[W, ?] =>
      OptionalEncoder(this, empty = CirceJson.Null).encode(json.self.self, w)
    case json: Json.Primitive[W, ?] => JsonPrimitiveCirceEncoder.encode(json, w)
    case json: Json.Record[W, ?]    =>
      CirceJson.fromFields(RecordEncoder(JsonFieldCirceEncoder).encode(json.self.self, w).toList)
    case json: Json.Tuple[W, ?] =>
      CirceJson.fromValues(TupleEncoder(this, empty = CirceJson.Null).encode(json.self.self, w))
    case json: Json.Union[W, ?] => UnionEncoder(JsonBranchCirceEncoder).encode(json.self.self, w)
