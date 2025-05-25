package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.schema.*

sealed abstract class Json[A] extends Product with Serializable

object Json:
  final case class Collection[A](self: Enrichment[Self.Collection[Json, *], A]) extends Json[A]

  object Collection:
    given EnrichedCollectionSchema[Json.Collection, Json] =
      EnrichedCollectionSchema[Enrichment[Self.Collection[Json, *], *], Json].imapK[Json.Collection](
        [A] => (schema: Enrichment[Self.Collection[Json, *], A]) => Collection(schema)
      )([A] => (json: Json.Collection[A]) => json.self)

  final case class Constant[A](self: Enrichment[Self.Constant[Json, *], A]) extends Json[A]

  object Constant:
    given EnrichedConstantSchema[Json.Constant, Json] =
      EnrichedConstantSchema[Enrichment[Self.Constant[Json, *], *], Json].imapK(
        [A] => (schema: Enrichment[Self.Constant[Json, *], A]) => Constant(schema)
      )([A] => (json: Json.Constant[A]) => json.self)

  final case class Dictionary[A](self: Enrichment[Self.Dictionary[Key, Json, *], A]) extends Json[A]

  object Dictionary:
    given EnrichedDictionarySchema[Json.Dictionary, Key, Json] =
      EnrichedDictionarySchema[Enrichment[Self.Dictionary[Key, Json, *], *], Key, Json].imapK(
        [A] => (schema: Enrichment[Self.Dictionary[Key, Json, *], A]) => Dictionary(schema)
      )([A] => (json: Json.Dictionary[A]) => json.self)

  final case class Enumeration[A](self: Enrichment[Self.Enumeration[Json.Primitive, *], A]) extends Json[A]

  object Enumeration:
    given EnrichedEnumerationSchema[Json.Enumeration, Json.Primitive] =
      EnrichedEnumerationSchema[Enrichment[Self.Enumeration[Json.Primitive, *], *], Json.Primitive].imapK(
        [A] => (schema: Enrichment[Self.Enumeration[Json.Primitive, *], A]) => Enumeration(schema)
      )([A] => (json: Json.Enumeration[A]) => json.self)

  final case class Nullable[A](self: Enrichment[Self.Nullable[Json, *], A]) extends Json[A]

  object Nullable:
    given EnrichedNullableSchema[Json.Nullable, Json] =
      EnrichedNullableSchema[Enrichment[Self.Nullable[Json, *], *], Json].imapK(
        [A] => (schema: Enrichment[Self.Nullable[Json, *], A]) => Nullable(schema)
      )([A] => (json: Json.Nullable[A]) => json.self)

  final case class Primitive[A](self: Enrichment[Self.Primitive, A]) extends Json[A]

  object Primitive:
    given EnrichedPrimitiveSchema[Json.Primitive] =
      EnrichedPrimitiveSchema[Enrichment[Self.Primitive, *]].imapK(
        [A] => (schema: Enrichment[Self.Primitive, A]) => Primitive(schema)
      )([A] => (json: Json.Primitive[A]) => json.self)

  final case class Record[A](self: Enrichment[Self.Record[Json.Field, *], A]) extends Json[A]

  object Record:
    given EnrichedRecordSchema[Json.Record, Json.Field] =
      EnrichedRecordSchema[Enrichment[Self.Record[Json.Field, *], *], Json.Field].imapK(
        [A] => (schema: Enrichment[Self.Record[Json.Field, *], A]) => Record(schema)
      )([A] => (json: Json.Record[A]) => json.self)

  final case class Tuple[A](self: Enrichment[Self.Tuple[Json, *], A]) extends Json[A]

  object Tuple:
    given EnrichedTupleSchema[Json.Tuple, Json] =
      EnrichedTupleSchema[Enrichment[Self.Tuple[Json, *], *], Json].imapK(
        [A] => (schema: Enrichment[Self.Tuple[Json, *], A]) => Tuple(schema)
      )([A] => (json: Json.Tuple[A]) => json.self)

  final case class Union[A](self: Enrichment[Self.Union[Json, *], A]) extends Json[A]

  object Union:
    given EnrichedUnionSchema[Json.Union, Json] =
      EnrichedUnionSchema[Enrichment[Self.Union[Json, *], *], Json].imapK(
        [A] => (schema: Enrichment[Self.Union[Json, *], A]) => Union(schema)
      )([A] => (json: Json.Union[A]) => json.self)

  final case class Field[A](self: Enrichment[Self.Field[Key, Json, *], A])

  object Field:
    given EnrichedFieldSchema[Json.Field, Key, Json] =
      EnrichedFieldSchema[Enrichment[Self.Field[Key, Json, *], *], Key, Json].imapK(
        [A] => (schema: Enrichment[Self.Field[Key, Json, *], A]) => Field(schema)
      )([A] => (json: Json.Field[A]) => json.self)

  given EnrichedSchema[Json] with
    extension [A](self: Json[A])
      override def metadata: Metadata = self match
        case Collection(schema)  => schema.metadata
        case Constant(schema)    => schema.metadata
        case Dictionary(schema)  => schema.metadata
        case Enumeration(schema) => schema.metadata
        case Nullable(schema)    => schema.metadata
        case Primitive(schema)   => schema.metadata
        case Record(schema)      => schema.metadata
        case Tuple(schema)       => schema.metadata
        case Union(schema)       => schema.metadata

      override def metadata(f: Metadata => Metadata): Json[A] = self match
        case Collection(schema)  => Collection(schema.copy(metadata = f(schema.metadata)))
        case Constant(schema)    => Constant(schema.copy(metadata = f(schema.metadata)))
        case Dictionary(schema)  => Dictionary(schema.copy(metadata = f(schema.metadata)))
        case Enumeration(schema) => Enumeration(schema.copy(metadata = f(schema.metadata)))
        case Nullable(schema)    => Nullable(schema.copy(metadata = f(schema.metadata)))
        case Primitive(schema)   => Primitive(schema.copy(metadata = f(schema.metadata)))
        case Record(schema)      => Record(schema.copy(metadata = f(schema.metadata)))
        case Tuple(schema)       => Tuple(schema.copy(metadata = f(schema.metadata)))
        case Union(schema)       => Union(schema.copy(metadata = f(schema.metadata)))

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
