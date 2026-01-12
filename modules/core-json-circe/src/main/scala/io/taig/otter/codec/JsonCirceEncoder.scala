package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json

object JsonCirceEncoder extends Encoder[Json.Write, CirceJson]:
  override def encode[A](schema: Json.Write[A], a: A): CirceJson = schema match
//     case Json.Coerce(annotation)     => CoerceEncoder(encoder = this).encode(schema = annotation.self, a)
//     case Json.Constant(annotation)   => ConstantEncoder(encoder = this).encode(schema = annotation.self, a)
    case self: Json.Collection.Write[A] =>
      CirceJson.fromValues(CollectionEncoder(encoder = this).encode(schema = self.self.self, a))
    case self: Json.Dictionary.Write[A] =>
      CirceJson.fromFields(DictionaryEncoder(encoder = this).encode(schema = self.self.self, a))
//     case Json.Enumeration(annotation) => EnumerationEncoder(encoder = this).encode(schema = annotation.self, a)
    case schema: Json.Primitive.Write[A] => JsonPrimitiveCirceEncoder.encode(schema, a)
    case self: Json.Record.Write[A]    => ???
    case self: Json.Tuple.Write[A]     => ???
//     case Json.Nullable(annotation)    =>
//       NullableEncoder(encoder = this, empty = CirceJson.Null).encode(schema = annotation.self, a)
//     case Json.Record(annotation)   =>
//       CirceJson.fromFields(RecordEncoder(encoder = JsonFieldCirceEncoder).encode(schema = annotation.self, a).toList)
//     case Json.Tuple(annotation) =>
//       CirceJson.fromValues(TupleEncoder(encoder = this, empty = CirceJson.Null).encode(schema = annotation.self, a))
//     case Json.Union(annotation) =>
//       UnionEncoder(encoder = JsonBranchCirceEncoder).encode(schema = annotation.self, a)
