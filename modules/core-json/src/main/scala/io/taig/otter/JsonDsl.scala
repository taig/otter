package io.taig.otter

import io.taig.otter.Json.Key

trait JsonDsl
    extends CollectionDsl[Json.Collection, Json],
      ConstantDsl[Json.Constant, Json],
      ConstantDsl.Primitive[Json.Constant, Json.Primitive],
      DictionaryDsl[Json.Dictionary, Json.Key, Json],
      EnumerationDsl[Json.Enumeration, Json.Primitive],
      OptionalDsl[Json.Optional, Json],
      PrimitiveDsl[Json.Primitive]
// RecordDsl[Json.Record, Json.Key, Json],
//   RecordDsl.Primitive.String[Json.Record, Json.Key, Json]:
// RecordDsl.Primitive.String[Json.Record, Json.Key, Json.Primitive],
// TupleDsl[Json.Tuple, Json],
// UnionDsl[Json.Union, Json]:
// override protected def fromCollection[A](self: Collection[Json, A]): Json.Collection[A] = Json.Collection(self)
// override protected def toCollection[A](codec: Json.Collection[A]): Collection[Json, A] = codec.self
// override protected def fromConstant[A](self: Constant[Json, A]): Json.Constant[A] = Json.Constant(self)
// override protected def fromDictionary[A](self: Dictionary[Json.Key, Json, A]): Json.Dictionary[A] =
//   Json.Dictionary(self)
// override protected def fromPrimitive[A](self: Primitive[A]): Json.Primitive[A] = Json.Primitive(self)
// override protected def fromEnumeration[A](self: Enumeration[Json.Primitive, A]): Json.Enumeration[A] =
//   Json.Enumeration(self)
// override protected def fromOptional[A](self: Optional[Json, A]): Json.Optional[A] = Json.Optional(self)
// // override protected def fromRecord[A](self: Record[Json.Key, Json, A]): Json.Record[A] = Json.Record(self)
// final override protected def fromRecord[A](self: Record[Json.Key, Json, A]): Json.Record[A] = ???
// override protected def fromTuple[A](self: Tuple[Json, A]): Json.Tuple[A] = Json.Tuple(self)
// override protected def fromUnion[A](self: Union[Json, A]): Json.Union[A] = Json.Union(self)
// override protected def toUnion[A](codec: Json.Union[A]): Union[Json, A] = codec.self

// final val key: JsonKeyDsl = JsonKeyDsl

object JsonDsl extends JsonDsl
