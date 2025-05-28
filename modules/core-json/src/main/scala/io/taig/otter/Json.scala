package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.operation.*
import cats.syntax.all.*

sealed abstract class Json[A] extends Product with Serializable

object Json:
  final case class Collection[A](self: Self.Collection[Json, A]) extends Json[A]

  object Collection:
    given CollectionSchemaInvariant[Json.Collection, Json] =
      CollectionSchemaInvariant[Self.Collection[Json, *], Json].imapK[Json.Collection](
        [A] => (schema: Self.Collection[Json, A]) => Collection(schema)
      )([A] => (json: Json.Collection[A]) => json.self)

  final case class Constant[A](self: Self.Constant[Json, A]) extends Json[A]

  object Constant:
    given ConstantSchemaInvariant[Json.Constant, Json] =
      ConstantSchemaInvariant[Self.Constant[Json, *], Json].imapK(
        [A] => (schema: Self.Constant[Json, A]) => Constant(schema)
      )([A] => (json: Json.Constant[A]) => json.self)

  final case class Dictionary[A](self: Self.Dictionary[Key, Json, A]) extends Json[A]

  object Dictionary:
    given DictionarySchemaInvariant[Json.Dictionary, Key, Json] =
      DictionarySchemaInvariant[Self.Dictionary[Key, Json, *], Key, Json].imapK(
        [A] => (schema: Self.Dictionary[Key, Json, A]) => Dictionary(schema)
      )([A] => (json: Json.Dictionary[A]) => json.self)

  final case class Enumeration[A](self: Self.Enumeration[Json.Primitive, A]) extends Json[A]

  object Enumeration:
    given EnumerationSchemaInvariant[Json.Enumeration, Json.Primitive] =
      EnumerationSchemaInvariant[Self.Enumeration[Json.Primitive, *], Json.Primitive].imapK(
        [A] => (schema: Self.Enumeration[Json.Primitive, A]) => Enumeration(schema)
      )([A] => (json: Json.Enumeration[A]) => json.self)

  final case class Nullable[A](self: Self.Nullable[Json, A]) extends Json[A]

  object Nullable:
    given NullableSchemaInvariant[Json.Nullable, Json] =
      NullableSchemaInvariant[Self.Nullable[Json, *], Json].imapK(
        [A] => (schema: Self.Nullable[Json, A]) => Nullable(schema)
      )([A] => (json: Json.Nullable[A]) => json.self)

  final case class Primitive[A](self: Self.Primitive[A]) extends Json[A]

  object Primitive:
    given PrimitiveSchemaInvariant[Json.Primitive] =
      PrimitiveSchemaInvariant[Self.Primitive].imapK(
        [A] => (schema: Self.Primitive[A]) => Primitive(schema)
      )([A] => (json: Json.Primitive[A]) => json.self)

  final case class Record[A](self: Self.Record[Json.Field, A]) extends Json[A]

  object Record:
    given RecordSchemaInvariant[Json.Record, Json.Field] =
      RecordSchemaInvariant[Self.Record[Json.Field, *], Json.Field].imapK(
        [A] => (schema: Self.Record[Json.Field, A]) => Record(schema)
      )([A] => (json: Json.Record[A]) => json.self)

  final case class Tuple[A](self: Self.Tuple[Json, A]) extends Json[A]

  object Tuple:
    given TupleSchemaInvariant[Json.Tuple, Json] =
      TupleSchemaInvariant[Self.Tuple[Json, *], Json].imapK(
        [A] => (schema: Self.Tuple[Json, A]) => Tuple(schema)
      )([A] => (json: Json.Tuple[A]) => json.self)

  final case class Union[A](self: Self.Union[Json, A]) extends Json[A]

  object Union:
    given UnionSchemaInvariant[Json.Union, Json] =
      UnionSchemaInvariant[Self.Union[Json, *], Json].imapK(
        [A] => (schema: Self.Union[Json, A]) => Union(schema)
      )([A] => (json: Json.Union[A]) => json.self)

  final case class Field[A](self: Self.Field[Key, Json, A])

  object Field:
    given FieldSchemaInvariant[Json.Field, Key, Json] =
      FieldSchemaInvariant[Self.Field[Key, Json, *], Key, Json].imapK(
        [A] => (schema: Self.Field[Key, Json, A]) => Field(schema)
      )([A] => (json: Json.Field[A]) => json.self)

  given SchemaInvariant[Json] with
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
        case Collection(schema)  => Collection(schema.metadata(f))
        case Constant(schema)    => Constant(schema.metadata(f))
        case Dictionary(schema)  => Dictionary(schema.metadata(f))
        case Enumeration(schema) => Enumeration(schema.metadata(f))
        case Nullable(schema)    => Nullable(schema.metadata(f))
        case Primitive(schema)   => Primitive(schema.metadata(f))
        case Record(schema)      => Record(schema.metadata(f))
        case Tuple(schema)       => Tuple(schema.metadata(f))
        case Union(schema)       => Union(schema.metadata(f))

    override def imap[A, B](fa: Json[A])(f: A => B)(g: B => A): Json[B] = fa match
      case Collection(self)  => Collection(self.imap(f)(g))
      case Constant(self)    => Constant(self.imap(f)(g))
      case Dictionary(self)  => Dictionary(self.imap(f)(g))
      case Enumeration(self) => Enumeration(self.imap(f)(g))
      case Nullable(self)    => Nullable(self.imap(f)(g))
      case Primitive(self)   => Primitive(self.imap(f)(g))
      case Record(self)      => Record(self.imap(f)(g))
      case Tuple(self)       => Tuple(self.imap(f)(g))
      case Union(self)       => Union(self.imap(f)(g))
