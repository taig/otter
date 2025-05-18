package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.schema.*

sealed abstract class Json[A] extends Product with Serializable

object Json:
  final case class Collection[A](self: Self.Collection[Json, A]) extends Json[A]

  object Collection:
    given CollectionSchema[Json.Collection, Json] = CollectionSchema[Self.Collection[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Collection[Json, A]) => Collection(schema)
      )([A] => (json: Json.Collection[A]) => json.self)

  final case class Constant[A](self: Self.Constant[Json, A]) extends Json[A]

  object Constant:
    given ConstantSchema[Json.Constant, Json] = ConstantSchema[Self.Constant[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Constant[Json, A]) => Constant(schema)
      )([A] => (json: Json.Constant[A]) => json.self)

  final case class Dictionary[A](self: Self.Dictionary[Key, Json, A]) extends Json[A]

  object Dictionary:
    given DictionarySchema[Json.Dictionary, Key, Json] =
      DictionarySchema[Self.Dictionary[Key, Json, *], Key, Json]
        .imapK(
          [A] => (schema: Self.Dictionary[Key, Json, A]) => Dictionary(schema)
        )([A] => (json: Json.Dictionary[A]) => json.self)

  final case class Enumeration[A](self: Self.Enumeration[Json.Primitive, A]) extends Json[A]

  object Enumeration:
    given EnumerationSchema[Json.Enumeration, Json.Primitive] =
      EnumerationSchema[Self.Enumeration[Json.Primitive, *], Json.Primitive]
        .imapK(
          [A] => (schema: Self.Enumeration[Json.Primitive, A]) => Enumeration(schema)
        )([A] => (json: Json.Enumeration[A]) => json.self)

  final case class Nullable[A](self: Self.Nullable[Json, A]) extends Json[A]

  object Nullable:
    given NullableSchema[Json.Nullable, Json] = NullableSchema[Self.Nullable[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Nullable[Json, A]) => Nullable(schema)
      )([A] => (json: Json.Nullable[A]) => json.self)

  final case class Primitive[A](self: Self.Primitive[A]) extends Json[A]

  object Primitive:
    given PrimitiveSchema[Json.Primitive] = PrimitiveSchema[Self.Primitive]
      .imapK(
        [A] => (schema: Self.Primitive[A]) => Primitive(schema)
      )([A] => (json: Json.Primitive[A]) => json.self)

  final case class Record[A](self: Self.Record[Json.Field, A]) extends Json[A]

  object Record:
    given RecordSchema[Json.Record, Json.Field] = RecordSchema[Self.Record[Json.Field, *], Json.Field]
      .imapK(
        [A] => (schema: Self.Record[Json.Field, A]) => Record(schema)
      )([A] => (json: Json.Record[A]) => json.self)

  final case class Sum[A](self: Self.Sum[Json.Branch, A]) extends Json[A]

  object Sum:
    given SumSchema[Json.Sum, Json.Branch] = SumSchema[Self.Sum[Json.Branch, *], Json.Branch]
      .imapK(
        [A] => (schema: Self.Sum[Json.Branch, A]) => Sum(schema)
      )([A] => (json: Json.Sum[A]) => json.self)

  final case class Tuple[A](self: Self.Tuple[Json, A]) extends Json[A]

  object Tuple:
    given TupleSchema[Json.Tuple, Json] = TupleSchema[Self.Tuple[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Tuple[Json, A]) => Tuple(schema)
      )([A] => (json: Json.Tuple[A]) => json.self)

  final case class Union[A](self: Self.Union[Json, A]) extends Json[A]

  object Union:
    given UnionSchema[Json.Union, Json] = UnionSchema[Self.Union[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Union[Json, A]) => Union(schema)
      )([A] => (json: Json.Union[A]) => json.self)

  final case class Branch[A](self: Self.Branch[Key, Json, A])

  object Branch:
    given BranchSchema[Json.Branch, Key, Json] = BranchSchema[Self.Branch[Key, Json, *], Key, Json]
      .imapK(
        [A] => (schema: Self.Branch[Key, Json, A]) => Branch(schema)
      )([A] => (json: Json.Branch[A]) => json.self)

  final case class Field[A](self: Self.Field[Key, Json, A])

  object Field:
    given FieldSchema[Json.Field, Key, Json] = FieldSchema[Self.Field[Key, Json, *], Key, Json]
      .imapK(
        [A] => (schema: Self.Field[Key, Json, A]) => Field(schema)
      )([A] => (json: Json.Field[A]) => json.self)
