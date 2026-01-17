package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json

object JsonCirceEncoder extends Encoder[Json.Write, CirceJson]:
  override def encode[A](schema: Json.Write[A], a: A): CirceJson = schema match
    case self: Json.Constant.Write[A]   => ConstantEncoder(encoder = this).encode(schema = self.self.self, a)
    case self: Json.Collection.Write[A] =>
      CirceJson.fromValues(CollectionEncoder(encoder = this).encode(schema = self.self.self, a))
    case self: Json.Dictionary.Write[A] =>
      CirceJson.fromFields(DictionaryEncoder(encoder = this).encode(schema = self.self.self, a))
    case self: Json.Enumeration.Write[A] => EnumerationEncoder(encoder = this).encode(schema = self.self.self, a)
    case schema: Json.Primitive.Write[A] => JsonPrimitiveCirceEncoder.encode(schema, a)
    case self: Json.Record.Write[A]      =>
      CirceJson.fromFields(RecordEncoder(encoder = JsonFieldCirceEncoder).encode(schema = self.self.self, a).toList)
    case self: Json.Optional.Write[A] =>
      OptionalEncoder(encoder = this, empty = CirceJson.Null).encode(schema = self.self.self, a)
    case self: Json.Tuple.Write[A] =>
      CirceJson.fromValues(TupleEncoder(encoder = this, empty = CirceJson.Null).encode(schema = self.self.self, a))
    case self: Json.Union.Write[A] =>
      UnionEncoder(encoder = JsonBranchCirceEncoder).encode(schema = self.self.self, a)
