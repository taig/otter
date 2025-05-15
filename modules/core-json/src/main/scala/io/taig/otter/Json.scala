package io.taig.otter

import io.taig.otter as Self

sealed abstract class Json[A] extends Product with Serializable

object Json:
  final case class Collection[A](self: Self.Collection[Json, A]) extends Json[A]

  object Collection:
    given Schema.Collection[Json.Collection, Json] = Schema
      .Collection[Self.Collection[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Collection[Json, A]) => Collection(schema)
      )([A] => (json: Json.Collection[A]) => json.self)

  final case class Constant[A](self: Self.Constant[Json, A]) extends Json[A]

  object Constant:
    given Schema.Constant[Json.Constant, Json] = Schema
      .Constant[Self.Constant[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Constant[Json, A]) => Constant(schema)
      )([A] => (json: Json.Constant[A]) => json.self)

  final case class Dictionary[A](self: Self.Dictionary[Json.Key, Json, A]) extends Json[A]

  object Dictionary:
    given Schema.Dictionary[Json.Dictionary, Json.Key, Json] = Schema
      .Dictionary[Self.Dictionary[Json.Key, Json, *], Json.Key, Json]
      .imapK(
        [A] => (schema: Self.Dictionary[Json.Key, Json, A]) => Dictionary(schema)
      )([A] => (json: Json.Dictionary[A]) => json.self)

  final case class Enumeration[A](self: Self.Enumeration[Json.Primitive, A]) extends Json[A]

  object Enumeration:
    given Schema.Enumeration[Json.Enumeration, Json.Primitive] = Schema
      .Enumeration[Self.Enumeration[Json.Primitive, *], Json.Primitive]
      .imapK(
        [A] => (schema: Self.Enumeration[Json.Primitive, A]) => Enumeration(schema)
      )([A] => (json: Json.Enumeration[A]) => json.self)

  final case class Nullable[A](self: Self.Nullable[Json, A]) extends Json[A]

  object Nullable:
    given Schema.Nullable[Json.Nullable, Json] = Schema
      .Nullable[Self.Nullable[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Nullable[Json, A]) => Nullable(schema)
      )([A] => (json: Json.Nullable[A]) => json.self)

  final case class Primitive[A](self: Self.Primitive[A]) extends Json[A]

  object Primitive:
    given Schema.Primitive[Json.Primitive] = Schema
      .Primitive[Self.Primitive]
      .imapK(
        [A] => (schema: Self.Primitive[A]) => Primitive(schema)
      )([A] => (json: Json.Primitive[A]) => json.self)

  final case class Record[A](self: Self.Record[Json.Field, A]) extends Json[A]

  object Record:
    given Schema.Record[Json.Record, Json.Field] = Schema
      .Record[Self.Record[Json.Field, *], Json.Field]
      .imapK(
        [A] => (schema: Self.Record[Json.Field, A]) => Record(schema)
      )([A] => (json: Json.Record[A]) => json.self)

  final case class Tuple[A](self: Self.Tuple[Json, A]) extends Json[A]

  object Tuple:
    given Schema.Tuple[Json.Tuple, Json] = Schema
      .Tuple[Self.Tuple[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Tuple[Json, A]) => Tuple(schema)
      )([A] => (json: Json.Tuple[A]) => json.self)

  final case class Union[A](self: Self.Union[Json, A]) extends Json[A]

  object Union:
    given Schema.Union[Json.Union, Json] = Schema
      .Union[Self.Union[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Union[Json, A]) => Union(schema)
      )([A] => (json: Json.Union[A]) => json.self)

  sealed abstract class Key[A] extends Product with Serializable

  object Key:
    final case class Constant[A](self: Self.Constant[Json.Key.Primitive, A]) extends Json.Key[A]

    object Constant:
      given Schema.Constant[Json.Key.Constant, Json.Key.Primitive] = Schema
        .Constant[Self.Constant[Json.Key.Primitive, *], Json.Key.Primitive]
        .imapK(
          [A] => (schema: Self.Constant[Json.Key.Primitive, A]) => Constant(schema)
        )([A] => (json: Json.Key.Constant[A]) => json.self)

    final case class Enumeration[A](self: Self.Enumeration[Json.Key.Primitive, A]) extends Json.Key[A]

    object Enumeration:
      given Schema.Enumeration[Json.Key.Enumeration, Json.Key.Primitive] = Schema
        .Enumeration[Self.Enumeration[Json.Key.Primitive, *], Json.Key.Primitive]
        .imapK(
          [A] => (schema: Self.Enumeration[Json.Key.Primitive, A]) => Enumeration(schema)
        )([A] => (json: Json.Key.Enumeration[A]) => json.self)

    final case class Primitive[A](self: Self.Primitive.String[A]) extends Json.Key[A]

    object Primitive:
      given Schema.Primitive.String[Json.Key.Primitive] = Schema.Primitive
        .String[Self.Primitive.String]
        .imapK(
          [A] => (schema: Self.Primitive.String[A]) => Primitive(schema)
        )([A] => (json: Json.Key.Primitive[A]) => json.self)

    final case class Union[A](self: Self.Union[Json.Key, A]) extends Json.Key[A]

    object Union:
      given Schema.Union[Json.Key.Union, Json.Key] = Schema
        .Union[Self.Union[Json.Key, *], Json.Key]
        .imapK(
          [A] => (schema: Self.Union[Json.Key, A]) => Union(schema)
        )([A] => (json: Json.Key.Union[A]) => json.self)

    given Schema[Json.Key] with
      extension [A](self: Key[A])
        override def metadata: Metadata = ???
        override def modifyMetadata(f: Metadata => Metadata): Key[A] = ???
        override def imap[B](f: A => B)(g: B => A): Key[B] = ???

  final case class Branch[A](self: Self.Branch[Json.Key, Json, A]) extends Json[A]

  object Branch:
    given Schema.Branch[Json.Branch, Json.Key, Json] = Schema
      .Branch[Self.Branch[Json.Key, Json, *], Json.Key, Json]
      .imapK(
        [A] => (schema: Self.Branch[Json.Key, Json, A]) => Branch(schema)
      )([A] => (json: Json.Branch[A]) => json.self)

  final case class Field[A](self: Self.Field[Json.Key, Json, A]) extends Json[A]

  object Field:
    given Schema.Field[Json.Field, Json.Key, Json] = Schema
      .Field[Self.Field[Json.Key, Json, *], Json.Key, Json]
      .imapK(
        [A] => (schema: Self.Field[Json.Key, Json, A]) => Field(schema)
      )([A] => (json: Json.Field[A]) => json.self)
