package io.taig.otter

import cats.~>
import cats.Order
import cats.Eq
import io.taig.otter.Json.Key

trait JsonDsl
    extends ConstantDsl[Json.Constant, Json],
      ConstantDsl.Primitive[Json.Constant, Json.Primitive],
      EnumerationDsl[Json.Enumeration, Json.Primitive],
      OptionalDsl[Json.Optional, Json],
      PrimitiveDsl[Json.Primitive],
      RecordDsl[Json.Record, Json.Key, Json],
      RecordDsl.Primitive.String[Json.Record, Json.Key, Json.Primitive],
      TupleDsl[Json.Tuple, Json],
      UnionDsl[Json.Union, Json]:
  override protected def fromConstant[A](self: Constant[Json, A]): Json.Constant[A] = Json.Constant(self)
  override protected def fromPrimitive[A](self: Primitive[A]): Json.Primitive[A] = Json.Primitive(self)
  override protected def fromEnumeration[A](self: Enumeration[Json.Primitive, A]): Json.Enumeration[A] =
    Json.Enumeration(self)
  override protected def fromOptional[A](self: Optional[Json, A]): Json.Optional[A] = Json.Optional(self)
  override protected def fromRecord[A](self: Record[Key, Json, A]): Json.Record[A] = Json.Record(self)
  override protected def fromTuple[A](self: Tuple[Json, A]): Json.Tuple[A] = Json.Tuple(self)
  override protected def fromUnion[A](self: Union[Json, A]): Json.Union[A] = Json.Union(self)
  override protected def toUnion[A](codec: Json.Union[A]): Union[Json, A] = codec.self

  object collection extends CollectionDsl[Json.Collection, Json]:
    override protected def fromCollection[A](self: Collection[Json, A]): Json.Collection[A] =
      Json.Collection(self)

  object dictionary extends DictionaryDsl[Json.Dictionary, Json.Key, Json]:
    override protected def fromDictionary[A](self: Dictionary[Json.Key, Json, A]): Json.Dictionary[A] =
      Json.Dictionary(self)

  override object key extends JsonKeyDsl

object JsonDsl extends JsonDsl
