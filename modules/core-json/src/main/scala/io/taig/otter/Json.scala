package io.taig.otter

import io.taig.otter.schema as Schema

sealed abstract class Json[A] extends Product with Serializable

object Json:
  final case class Collection[A](self: Schema.Collection[Json, A]) extends Json[A]

  object Collection:
    given Shape.Collection[Json.Collection, Json] = Shape
      .Collection[Schema.Collection[Json, *], Json]
      .imapK(
        [A] => (schema: Schema.Collection[Json, A]) => Collection(schema)
      )([A] => (json: Json.Collection[A]) => json.self)

  final case class Constant[A](self: Schema.Constant[Json, A]) extends Json[A]

  object Constant:
    given Shape.Constant[Json.Constant, Json] = Shape
      .Constant[Schema.Constant[Json, *], Json]
      .imapK(
        [A] => (schema: Schema.Constant[Json, A]) => Constant(schema)
      )([A] => (json: Json.Constant[A]) => json.self)

  final case class Dictionary[A](self: Schema.Dictionary[Json.Key, Json, A]) extends Json[A]

  object Dictionary:
    given Shape.Dictionary[Json.Dictionary, Json.Key, Json] = Shape
      .Dictionary[Schema.Dictionary[Json.Key, Json, *], Json.Key, Json]
      .imapK(
        [A] => (schema: Schema.Dictionary[Json.Key, Json, A]) => Dictionary(schema)
      )([A] => (json: Json.Dictionary[A]) => json.self)

  final case class Enumeration[A](self: Schema.Enumeration[Json.Primitive, A]) extends Json[A]

  object Enumeration:
    given Shape.Enumeration[Json.Enumeration, Json.Primitive] = Shape
      .Enumeration[Schema.Enumeration[Json.Primitive, *], Json.Primitive]
      .imapK(
        [A] => (schema: Schema.Enumeration[Json.Primitive, A]) => Enumeration(schema)
      )([A] => (json: Json.Enumeration[A]) => json.self)

  final case class Nullable[A](self: Schema.Nullable[Json, A]) extends Json[A]

  object Nullable:
    given Shape.Nullable[Json.Nullable, Json] = Shape
      .Nullable[Schema.Nullable[Json, *], Json]
      .imapK(
        [A] => (schema: Schema.Nullable[Json, A]) => Nullable(schema)
      )([A] => (json: Json.Nullable[A]) => json.self)

  final case class Primitive[A](self: Schema.Primitive[A]) extends Json[A]

  object Primitive:
    given Shape.Primitive[Json.Primitive] = Shape
      .Primitive[Schema.Primitive]
      .imapK(
        [A] => (schema: Schema.Primitive[A]) => Primitive(schema)
      )([A] => (json: Json.Primitive[A]) => json.self)

  final case class Record[A](self: Schema.Record[Json.Field, A]) extends Json[A]

  object Record:
    given Shape.Record[Json.Record, Json.Field] = Shape
      .Record[Schema.Record[Json.Field, *], Json.Field]
      .imapK(
        [A] => (schema: Schema.Record[Json.Field, A]) => Record(schema)
      )([A] => (json: Json.Record[A]) => json.self)

  final case class Tuple[A](self: Schema.Tuple[Json, A]) extends Json[A]

  object Tuple:
    given Shape.Tuple[Json.Tuple, Json] = Shape
      .Tuple[Schema.Tuple[Json, *], Json]
      .imapK(
        [A] => (schema: Schema.Tuple[Json, A]) => Tuple(schema)
      )([A] => (json: Json.Tuple[A]) => json.self)

  final case class Union[A](self: Schema.Union[Json, A]) extends Json[A]

  object Union:
    given Shape.Union[Json.Union, Json] = Shape
      .Union[Schema.Union[Json, *], Json]
      .imapK(
        [A] => (schema: Schema.Union[Json, A]) => Union(schema)
      )([A] => (json: Json.Union[A]) => json.self)

  sealed abstract class Key[A] extends Product with Serializable

  object Key:
    final case class Constant[A](self: Schema.Constant[Json.Key.Primitive, A]) extends Json.Key[A]

    object Constant:
      given Shape.Constant[Json.Key.Constant, Json.Key.Primitive] = Shape
        .Constant[Schema.Constant[Json.Key.Primitive, *], Json.Key.Primitive]
        .imapK(
          [A] => (schema: Schema.Constant[Json.Key.Primitive, A]) => Constant(schema)
        )([A] => (json: Json.Key.Constant[A]) => json.self)

    final case class Enumeration[A](self: Schema.Enumeration[Json.Key.Primitive, A]) extends Json.Key[A]

    object Enumeration:
      given Shape.Enumeration[Json.Key.Enumeration, Json.Key.Primitive] = Shape
        .Enumeration[Schema.Enumeration[Json.Key.Primitive, *], Json.Key.Primitive]
        .imapK(
          [A] => (schema: Schema.Enumeration[Json.Key.Primitive, A]) => Enumeration(schema)
        )([A] => (json: Json.Key.Enumeration[A]) => json.self)

    final case class Primitive[A](self: Schema.Primitive.String[A]) extends Json.Key[A]

    object Primitive:
      given Shape.Primitive.String[Json.Key.Primitive] = Shape.Primitive
        .String[Schema.Primitive.String]
        .imapK(
          [A] => (schema: Schema.Primitive.String[A]) => Primitive(schema)
        )([A] => (json: Json.Key.Primitive[A]) => json.self)

    final case class Union[A](self: Schema.Union[Json.Key, A]) extends Json.Key[A]

    object Union:
      given Shape.Union[Json.Key.Union, Json.Key] = Shape
        .Union[Schema.Union[Json.Key, *], Json.Key]
        .imapK(
          [A] => (schema: Schema.Union[Json.Key, A]) => Union(schema)
        )([A] => (json: Json.Key.Union[A]) => json.self)

    given Shape[Json.Key] with
      extension [A](self: Key[A])
        override def metadata: Metadata = ???
        override def modifyMetadata(f: Metadata => Metadata): Key[A] = ???
        override def imap[B](f: A => B)(g: B => A): Key[B] = ???

  final case class Branch[A](self: Schema.Branch[Json.Key, Json, A]) extends Json[A]

  object Branch:
    given Shape.Branch[Json.Branch, Json.Key, Json] = Shape
      .Branch[Schema.Branch[Json.Key, Json, *], Json.Key, Json]
      .imapK(
        [A] => (schema: Schema.Branch[Json.Key, Json, A]) => Branch(schema)
      )([A] => (json: Json.Branch[A]) => json.self)

  final case class Field[A](self: Schema.Field[Json.Key, Json, A]) extends Json[A]

  object Field:
    given Shape.Field[Json.Field, Json.Key, Json] = Shape
      .Field[Schema.Field[Json.Key, Json, *], Json.Key, Json]
      .imapK(
        [A] => (schema: Schema.Field[Json.Key, Json, A]) => Field(schema)
      )([A] => (json: Json.Field[A]) => json.self)
