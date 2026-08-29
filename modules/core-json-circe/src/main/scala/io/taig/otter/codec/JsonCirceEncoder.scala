package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json

object JsonCirceEncoder extends Encoder[Json.Node, CirceJson]:
  override def encode[W](json: Json.Node[W, Any], w: W): CirceJson = json match
    case Json.Coerce.Schema(node)                => CoerceEncoder(JsonPrimitiveCirceEncoder).encode(node.self, w)
    case Json.Collection.Schema(node)            => CirceJson.fromValues(CollectionEncoder(this).encode(node.self, w))
    case Json.Constant.Schema(node)              => ConstantEncoder(JsonPrimitiveCirceEncoder).encode(node.self, w)
    case Json.Dictionary.Schema(node)            => CirceJson.fromFields(DictionaryEncoder(this).encode(node.self, w))
    case Json.Enumeration.Schema(node)           => EnumerationEncoder(JsonPrimitiveCirceEncoder).encode(node.self, w)
    case Json.Optional.Schema(node)              => OptionalEncoder(this, empty = CirceJson.Null).encode(node.self, w)
    case json @ Json.Primitive.Boolean.Schema(_) => JsonPrimitiveCirceEncoder.encode(json, w)
    case json @ Json.Primitive.Number.Schema(_)  => JsonPrimitiveCirceEncoder.encode(json, w)
    case json @ Json.Primitive.Text.Schema(_)    => JsonPrimitiveCirceEncoder.encode(json, w)
    case Json.Record.Schema(node)                =>
      CirceJson.fromFields(RecordEncoder(JsonFieldCirceEncoder).encode(node.self, w).toList)
    case Json.Tuple.Schema(node) =>
      CirceJson.fromValues(TupleEncoder(this, empty = CirceJson.Null).encode(node.self, w))
    case Json.Union.Schema(node) => UnionEncoder(JsonBranchCirceEncoder).encode(node.self, w)
