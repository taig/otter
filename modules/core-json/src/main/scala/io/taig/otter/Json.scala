package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.schema.*

sealed abstract class Json[A] extends Product with Serializable

object Json:
  final case class Collection[A](self: Enriched[Self.Collection[Json, *], A]) extends Json[A]

  object Collection:
    given CollectionSchema[Json.Collection, Json] = CollectionSchema[Self.Collection[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Collection[Json, A]) => Collection(Enriched(schema, metadata = Metadata.Empty))
      )([A] => (json: Json.Collection[A]) => json.self.self)

  final case class Constant[A](self: Enriched[Self.Constant[Json, *], A]) extends Json[A]

  object Constant:
    given ConstantSchema[Json.Constant, Json] = ConstantSchema[Self.Constant[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Constant[Json, A]) => Constant(Enriched(schema))
      )([A] => (json: Json.Constant[A]) => json.self.self)

  final case class Dictionary[A](self: Enriched[Self.Dictionary[Key, Json, *], A]) extends Json[A]

  object Dictionary:
    given DictionarySchema[Json.Dictionary, Key, Json] =
      DictionarySchema[Self.Dictionary[Key, Json, *], Key, Json]
        .imapK(
          [A] => (schema: Self.Dictionary[Key, Json, A]) => Dictionary(Enriched(schema))
        )([A] => (json: Json.Dictionary[A]) => json.self.self)

  final case class Enumeration[A](self: Enriched[Self.Enumeration[Json.Primitive, *], A]) extends Json[A]

  object Enumeration:
    given EnumerationSchema[Json.Enumeration, Json.Primitive] =
      EnumerationSchema[Self.Enumeration[Json.Primitive, *], Json.Primitive]
        .imapK(
          [A] => (schema: Self.Enumeration[Json.Primitive, A]) => Enumeration(Enriched(schema))
        )([A] => (json: Json.Enumeration[A]) => json.self.self)

  final case class Nullable[A](self: Enriched[Self.Nullable[Json, *], A]) extends Json[A]

  object Nullable:
    given NullableSchema[Json.Nullable, Json] = NullableSchema[Self.Nullable[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Nullable[Json, A]) => Nullable(Enriched(schema))
      )([A] => (json: Json.Nullable[A]) => json.self.self)

  final case class Primitive[A](self: Enriched[Self.Primitive, A]) extends Json[A]

  object Primitive:
    given PrimitiveSchema[Json.Primitive] = PrimitiveSchema[Self.Primitive]
      .imapK(
        [A] => (schema: Self.Primitive[A]) => Primitive(Enriched(schema))
      )([A] => (json: Json.Primitive[A]) => json.self.self)

  final case class Record[A](self: Enriched[Self.Record[Json.Field, *], A]) extends Json[A]

  object Record:
    given RecordSchema[Json.Record, Json.Field] = RecordSchema[Self.Record[Json.Field, *], Json.Field]
      .imapK(
        [A] => (schema: Self.Record[Json.Field, A]) => Record(Enriched(schema))
      )([A] => (json: Json.Record[A]) => json.self.self)

  final case class Tuple[A](self: Enriched[Self.Tuple[Json, *], A]) extends Json[A]

  object Tuple:
    given TupleSchema[Json.Tuple, Json] = TupleSchema[Self.Tuple[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Tuple[Json, A]) => Tuple(Enriched(schema))
      )([A] => (json: Json.Tuple[A]) => json.self.self)

  final case class Union[A](self: Enriched[Self.Union[Json, *], A]) extends Json[A]

  object Union:
    given UnionSchema[Json.Union, Json] = UnionSchema[Self.Union[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Union[Json, A]) => Union(Enriched(schema))
      )([A] => (json: Json.Union[A]) => json.self.self)

  final case class Field[A](self: Enriched[Self.Field[Key, Json, *], A])

  object Field:
    given FieldSchema[Json.Field, Key, Json, Json.Record] =
      FieldSchema[Self.Field[Key, Json, *], Key, Json, Json.Record]
        .imapK(
          [A] => (schema: Self.Field[Key, Json, A]) => Field(Enriched(schema))
        )([A] => (json: Json.Field[A]) => json.self.self)

  given Schema[Json] with
    override def imap[A, B](fa: Json[A])(f: A => B)(g: B => A): Json[B] = fa match
      case Collection(self)  => Collection(self.mapF(_.imap(f)(g)))
      case Constant(self)    => Constant(self.mapF(_.imap(f)(g)))
      case Dictionary(self)  => Dictionary(self.mapF(_.imap(f)(g)))
      case Enumeration(self) => Enumeration(self.mapF(_.imap(f)(g)))
      case Nullable(self)    => Nullable(self.mapF(_.imap(f)(g)))
      case Primitive(self)   => Primitive(self.mapF(_.imap(f)(g)))
      case Record(self)      => Record(self.mapF(_.imap(f)(g)))
      case Tuple(self)       => Tuple(self.mapF(_.imap(f)(g)))
      case Union(self)       => Union(self.mapF(_.imap(f)(g)))