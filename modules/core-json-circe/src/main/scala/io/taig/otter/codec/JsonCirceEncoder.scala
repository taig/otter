package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json

object JsonCirceEncoder extends Encoder[Json.Node, CirceJson]:
  override def encode[W](json: Json.Node[W, Any], w: W): CirceJson = (json: @unchecked) match
    case json: Json.Coerce.Node[W, ?]     => CoerceEncoder(JsonPrimitiveCirceEncoder).encode(json.self.self, w)
    case json: Json.Collection.Node[W, ?] =>
      CirceJson.fromValues(CollectionEncoder(this).encode(json.self.self, w))
    case json: Json.Constant.Node[W, ?]   => ConstantEncoder(JsonPrimitiveCirceEncoder).encode(json.self.self, w)
    case json: Json.Dictionary.Node[W, ?] =>
      CirceJson.fromFields(DictionaryEncoder(this).encode(json.self.self, w))
    case json: Json.Enumeration.Node[W, ?] =>
      EnumerationEncoder(JsonPrimitiveCirceEncoder).encode(json.self.self, w)
    case json: Json.Optional.Node[W, ?] =>
      OptionalEncoder(this, empty = CirceJson.Null).encode(json.self.self, w)
    case json: Json.Primitive.Node[W, ?] => JsonPrimitiveCirceEncoder.encode(json, w)
    case json: Json.Record.Node[W, ?]    =>
      CirceJson.fromFields(RecordEncoder(JsonFieldCirceEncoder).encode(json.self.self, w).toList)
    case json: Json.Tuple.Node[W, ?] =>
      CirceJson.fromValues(TupleEncoder(this, empty = CirceJson.Null).encode(json.self.self, w))
    case json: Json.Union.Node[W, ?] => UnionEncoder(JsonBranchCirceEncoder).encode(json.self.self, w)
