package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.schema.*

sealed abstract class Json[A] extends Product with Serializable

object Json:
  final case class Collection[A](self: Enriched[Self.Collection[Json, *], A]) extends Json[A]

  object Collection:
    given CollectionSchema[Json.Collection, Json] = CollectionSchema[Self.Collection[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Collection[Json, A]) => Collection(Enriched(schema))
      )([A] => (json: Json.Collection[A]) => json.self.self)

    given EnrichedSchema[Json.Collection] = EnrichedSchema[Enriched[Self.Collection[Json, *], *]]
      .imapK[Json.Collection](
        [A] => (schema: Enriched[Self.Collection[Json, *], A]) => Json.Collection(schema)
      )([A] => (json: Json.Collection[A]) => json.self)

  final case class Constant[A](self: Enriched[Self.Constant[Json, *], A]) extends Json[A]

  object Constant:
    given ConstantSchema[Json.Constant, Json] = ConstantSchema[Self.Constant[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Constant[Json, A]) => Constant(Enriched(schema))
      )([A] => (json: Json.Constant[A]) => json.self.self)

    given EnrichedSchema[Json.Constant] = EnrichedSchema[Enriched[Self.Constant[Json, *], *]]
      .imapK[Json.Constant](
        [A] => (schema: Enriched[Self.Constant[Json, *], A]) => Json.Constant(schema)
      )([A] => (json: Json.Constant[A]) => json.self)

  final case class Dictionary[A](self: Enriched[Self.Dictionary[Key, Json, *], A]) extends Json[A]

  object Dictionary:
    given DictionarySchema[Json.Dictionary, Key, Json] =
      DictionarySchema[Self.Dictionary[Key, Json, *], Key, Json]
        .imapK(
          [A] => (schema: Self.Dictionary[Key, Json, A]) => Dictionary(Enriched(schema))
        )([A] => (json: Json.Dictionary[A]) => json.self.self)

    given EnrichedSchema[Json.Dictionary] = EnrichedSchema[Enriched[Self.Dictionary[Key, Json, *], *]]
      .imapK[Json.Dictionary](
        [A] => (schema: Enriched[Self.Dictionary[Key, Json, *], A]) => Json.Dictionary(schema)
      )([A] => (json: Json.Dictionary[A]) => json.self)

  final case class Enumeration[A](self: Enriched[Self.Enumeration[Json.Primitive, *], A]) extends Json[A]

  object Enumeration:
    given EnumerationSchema[Json.Enumeration, Json.Primitive] =
      EnumerationSchema[Self.Enumeration[Json.Primitive, *], Json.Primitive]
        .imapK(
          [A] => (schema: Self.Enumeration[Json.Primitive, A]) => Enumeration(Enriched(schema))
        )([A] => (json: Json.Enumeration[A]) => json.self.self)

    given EnrichedSchema[Json.Enumeration] = EnrichedSchema[Enriched[Self.Enumeration[Json.Primitive, *], *]]
      .imapK[Json.Enumeration](
        [A] => (schema: Enriched[Self.Enumeration[Json.Primitive, *], A]) => Json.Enumeration(schema)
      )([A] => (json: Json.Enumeration[A]) => json.self)

  final case class Nullable[A](self: Enriched[Self.Nullable[Json, *], A]) extends Json[A]

  object Nullable:
    given NullableSchema[Json.Nullable, Json] = NullableSchema[Self.Nullable[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Nullable[Json, A]) => Nullable(Enriched(schema))
      )([A] => (json: Json.Nullable[A]) => json.self.self)

    given EnrichedSchema[Json.Nullable] = EnrichedSchema[Enriched[Self.Nullable[Json, *], *]]
      .imapK[Json.Nullable](
        [A] => (schema: Enriched[Self.Nullable[Json, *], A]) => Json.Nullable(schema)
      )([A] => (json: Json.Nullable[A]) => json.self)

  final case class Primitive[A](self: Enriched[Self.Primitive, A]) extends Json[A]

  object Primitive:
    given PrimitiveSchema[Json.Primitive] = PrimitiveSchema[Self.Primitive]
      .imapK(
        [A] => (schema: Self.Primitive[A]) => Primitive(Enriched(schema))
      )([A] => (json: Json.Primitive[A]) => json.self.self)

    given EnrichedSchema[Json.Primitive] = EnrichedSchema[Enriched[Self.Primitive, *]]
      .imapK[Json.Primitive](
        [A] => (schema: Enriched[Self.Primitive, A]) => Json.Primitive(schema)
      )([A] => (json: Json.Primitive[A]) => json.self)

  final case class Record[A](self: Enriched[Self.Record[Json.Field, *], A]) extends Json[A]

  object Record:
    given RecordSchema[Json.Record, Json.Field] = RecordSchema[Enriched[Self.Record[Json.Field, *], *], Json.Field]
      .imapK(
        [A] => (schema: Enriched[Self.Record[Json.Field, *], A]) => Record(schema)
      )([A] => (json: Json.Record[A]) => json.self)

    given EnrichedSchema[Json.Record] = EnrichedSchema[Enriched[Self.Record[Json.Field, *], *]]
      .imapK[Json.Record](
        [A] => (schema: Enriched[Self.Record[Json.Field, *], A]) => Json.Record(schema)
      )([A] => (json: Json.Record[A]) => json.self)

  final case class Tuple[A](self: Enriched[Self.Tuple[Json, *], A]) extends Json[A]

  object Tuple:
    given TupleSchema[Json.Tuple, Json] = TupleSchema[Self.Tuple[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Tuple[Json, A]) => Tuple(Enriched(schema))
      )([A] => (json: Json.Tuple[A]) => json.self.self)

    given EnrichedSchema[Json.Tuple] = EnrichedSchema[Enriched[Self.Tuple[Json, *], *]]
      .imapK[Json.Tuple](
        [A] => (schema: Enriched[Self.Tuple[Json, *], A]) => Json.Tuple(schema)
      )([A] => (json: Json.Tuple[A]) => json.self)

  final case class Union[A](self: Enriched[Self.Union[Json, *], A]) extends Json[A]

  object Union:
    given UnionSchema[Json.Union, Json] = UnionSchema[Self.Union[Json, *], Json]
      .imapK(
        [A] => (schema: Self.Union[Json, A]) => Union(Enriched(schema))
      )([A] => (json: Json.Union[A]) => json.self.self)

    given EnrichedSchema[Json.Union] = EnrichedSchema[Enriched[Self.Union[Json, *], *]]
      .imapK[Json.Union](
        [A] => (schema: Enriched[Self.Union[Json, *], A]) => Json.Union(schema)
      )([A] => (json: Json.Union[A]) => json.self)

  final case class Field[A](self: Enriched[Self.Field[Key, Json, *], A])

  object Field:
    given schema: FieldSchema[Json.Field, Key, Json, Json.Record] =
      FieldSchema[Self.Field[Key, Json, *], Key, Json, Json.Record]
        .imapK(
          [A] => (schema: Self.Field[Key, Json, A]) => Field(Enriched(schema))
        )([A] => (json: Json.Field[A]) => json.self.self)

    given EnrichedSchema[Json.Field] = EnrichedSchema[Enriched[Self.Field[Key, Json, *], *]]
      .imapK[Json.Field](
        [A] => (schema: Enriched[Self.Field[Key, Json, *], A]) => Json.Field(schema)
      )([A] => (json: Json.Field[A]) => json.self)

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
