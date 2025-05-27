package io.taig.otter.http

import io.taig.otter as Self
import io.taig.otter.*
import org.typelevel.ci.CIString
import io.taig.otter.operation.*
import cats.syntax.all.*

final case class Header[A](self: Enrichment[Header.Value[A]]) extends AnyVal:
  inline def value: Header.Value[A] = self.self

  def name: CIString = value.name
  def schema: Header.Schema[?] = value.schema.value
  def isOptional: Boolean = value.isOptional
  def optional: Header[Option[A]] = Header(Enrichment(value.optional))

  def :*[B](header: Header[B])(using merge: Merge[A, B]): Headers[merge.Out] = toHeaders :* header

  def *:[B](header: Header[B])(using merge: Merge[B, A]): Headers[merge.Out] =
    header.toHeaders * toHeaders

  def toHeaders: Headers[A] = Headers(Enrichment(Headers.Value.Root(this)))

object Header:
  sealed abstract class Value[A] extends Product, Serializable:
    def name: CIString

    def schema: Reference[Header.Schema, ?]

    def isOptional: Boolean

    final def imap[B](f: A => B)(g: B => A): Value[B] = Value.Modify(self = this, f, g)

    final def optional: Header.Value[Option[A]] = Value.Optional(self = this)

  object Value:
    final private[otter] case class Root[A](name: CIString, schema: Reference[Header.Schema, A]) extends Value[A]:
      override def isOptional: Boolean = false

    final private[otter] case class Optional[A](self: Value[A]) extends Value[Option[A]]:
      export self.{name, schema}
      override def isOptional: Boolean = true

    final private[otter] case class Modify[A, B](self: Value[A], f: A => B, g: B => A) extends Value[B]:
      export self.{isOptional, name, schema}

  sealed trait Schema[A] extends Product with Serializable

  object Schema:
    sealed trait Value[A] extends Header.Schema[A], Header.Schema.Object.Value[A]

    object Value:
      final case class Constant[A](self: Enrichment[Self.Constant[Header.Schema.Value.Primitive, A]])
          extends Header.Schema.Value[A]

      object Constant:
        given EnrichedConstantSchemaInvariant[Header.Schema.Value.Constant, Header.Schema.Value.Primitive] =
          EnrichedConstantSchemaInvariant[
            [a] =>> Enrichment[Self.Constant[Header.Schema.Value.Primitive, a]],
            Header.Schema.Value.Primitive
          ].imapK(
            [A] => (schema: Enrichment[Self.Constant[Header.Schema.Value.Primitive, A]]) => Constant(schema)
          )([A] => (value: Header.Schema.Value.Constant[A]) => value.self)

      final case class Enumeration[A](self: Enrichment[Self.Enumeration[Header.Schema.Value.Primitive, A]])
          extends Header.Schema.Value[A]

      object Enumeration:
        given EnrichedEnumerationSchemaInvariant[Header.Schema.Value.Enumeration, Header.Schema.Value.Primitive] =
          EnrichedEnumerationSchemaInvariant[
            [a] =>> Enrichment[Self.Enumeration[Header.Schema.Value.Primitive, a]],
            Header.Schema.Value.Primitive
          ].imapK(
            [A] => (schema: Enrichment[Self.Enumeration[Header.Schema.Value.Primitive, A]]) => Enumeration(schema)
          )([A] => (value: Header.Schema.Value.Enumeration[A]) => value.self)

      final case class Primitive[A](self: Enrichment[Self.Primitive.String[A]]) extends Header.Schema.Value[A]

      object Primitive:
        given EnrichedPrimitiveSchemaInvariant.String[Header.Schema.Value.Primitive] =
          EnrichedPrimitiveSchemaInvariant
            .String[[a] =>> Enrichment[Self.Primitive.String[a]]]
            .imapK(
              [A] => (schema: Enrichment[Self.Primitive.String[A]]) => Primitive(schema)
            )([A] => (value: Header.Schema.Value.Primitive[A]) => value.self)

      final case class Union[A](self: Enrichment[Self.Union[Header.Schema.Value, A]]) extends Header.Schema.Value[A]

      object Union:
        given EnrichedUnionSchemaInvariant[Header.Schema.Value.Union, Header.Schema.Value] =
          EnrichedUnionSchemaInvariant[[a] =>> Enrichment[Self.Union[Header.Schema.Value, a]], Header.Schema.Value]
            .imapK(
              [A] => (schema: Enrichment[Self.Union[Header.Schema.Value, A]]) => Union(schema)
            )([A] => (value: Header.Schema.Value.Union[A]) => value.self)

      given EnrichedSchemaInvariant[Header.Schema.Value] with
        override def imap[A, B](fa: Header.Schema.Value[A])(f: A => B)(g: B => A): Header.Schema.Value[B] = fa match
          case Constant(self)    => Constant(self.map(_.imap(f)(g)))
          case Enumeration(self) => Enumeration(self.map(_.imap(f)(g)))
          case Primitive(self)   => Primitive(self.map(_.imap(f)(g)))
          case Union(self)       => Union(self.map(_.imap(f)(g)))

        extension [A](self: Header.Schema.Value[A])
          override def metadata: Metadata = self match
            case Constant(self)    => self.metadata
            case Enumeration(self) => self.metadata
            case Primitive(self)   => self.metadata
            case Union(self)       => self.metadata
          override def metadata(f: Metadata => Metadata): Header.Schema.Value[A] = self match
            case Constant(self)    => Constant(self.copy(metadata = f(self.metadata)))
            case Enumeration(self) => Enumeration(self.copy(metadata = f(self.metadata)))
            case Primitive(self)   => Primitive(self.copy(metadata = f(self.metadata)))
            case Union(self)       => Union(self.copy(metadata = f(self.metadata)))

    sealed trait Array[A] extends Header.Schema[A]

    object Array:
      final case class Collection[A](self: Enrichment[Self.Collection[Header.Schema.Value, A]])
          extends Header.Schema.Array[A]

      object Collection:
        given EnrichedCollectionSchemaInvariant[Header.Schema.Array.Collection, Header.Schema.Value] =
          EnrichedCollectionSchemaInvariant[[a] =>> Enrichment[
            Self.Collection[Header.Schema.Value, a]
          ], Header.Schema.Value]
            .imapK(
              [A] => (schema: Enrichment[Self.Collection[Header.Schema.Value, A]]) => Collection(schema)
            )([A] => (value: Header.Schema.Array.Collection[A]) => value.self)

      final case class Tuple[A](self: Enrichment[Self.Tuple[Header.Schema.Value, A]]) extends Header.Schema.Array[A]

      object Tuple:
        given EnrichedTupleSchemaInvariant[Header.Schema.Array.Tuple, Header.Schema.Value] =
          EnrichedTupleSchemaInvariant[[a] =>> Enrichment[Self.Tuple[Header.Schema.Value, a]], Header.Schema.Value]
            .imapK(
              [A] => (schema: Enrichment[Self.Tuple[Header.Schema.Value, A]]) => Tuple(schema)
            )([A] => (value: Header.Schema.Array.Tuple[A]) => value.self)

      given EnrichedSchemaInvariant[Header.Schema.Array] with
        override def imap[A, B](fa: Header.Schema.Array[A])(f: A => B)(g: B => A): Header.Schema.Array[B] = fa match
          case Collection(self) => Collection(self.map(_.imap(f)(g)))
          case Tuple(self)      => Tuple(self.map(_.imap(f)(g)))

        extension [A](self: Array[A])
          override def metadata: Metadata = self match
            case Collection(self) => self.metadata
            case Tuple(self)      => self.metadata

          override def metadata(f: Metadata => Metadata): Array[A] = self match
            case Collection(self) => Collection(self.copy(metadata = f(self.metadata)))
            case Tuple(self)      => Tuple(self.copy(metadata = f(self.metadata)))

    sealed trait Object[A] extends Header.Schema[A]

    object Object:
      final case class Dictionary[A](self: Enrichment[Self.Dictionary[Key, Header.Schema.Object.Value, A]])
          extends Header.Schema.Object[A]

      object Dictionary:
        given EnrichedDictionarySchemaInvariant[Header.Schema.Object.Dictionary, Key, Header.Schema.Value] =
          EnrichedDictionarySchemaInvariant[[a] =>> Enrichment[
            Self.Dictionary[Key, Header.Schema.Object.Value, a]
          ], Key, Header.Schema.Value]
            .imapK(
              [A] => (schema: Enrichment[Self.Dictionary[Key, Header.Schema.Object.Value, A]]) => Dictionary(schema)
            )([A] => (value: Header.Schema.Object.Dictionary[A]) => value.self)

      final case class Record[A](self: Enrichment[Self.Record[Header.Schema.Field, A]]) extends Header.Schema.Object[A]

      object Record:
        given EnrichedRecordSchemaInvariant[Header.Schema.Object.Record, Header.Schema.Field] =
          EnrichedRecordSchemaInvariant[[a] =>> Enrichment[Self.Record[Header.Schema.Field, a]], Header.Schema.Field]
            .imapK(
              [A] => (schema: Enrichment[Self.Record[Header.Schema.Field, A]]) => Record(schema)
            )([A] => (value: Header.Schema.Object.Record[A]) => value.self)

      sealed trait Value[A] extends Product with Serializable

      object Value:
        final case class Nullable[A](self: Enrichment[Self.Nullable[Header.Schema.Object.Value, A]])
            extends Header.Schema.Object.Value[A]

        object Nullable:
          given EnrichedNullableSchemaInvariant[Header.Schema.Object.Value.Nullable, Header.Schema.Object.Value] =
            EnrichedNullableSchemaInvariant[[a] =>> Enrichment[
              Self.Nullable[Header.Schema.Object.Value, a]
            ], Header.Schema.Object.Value]
              .imapK(
                [A] => (schema: Enrichment[Self.Nullable[Header.Schema.Object.Value, A]]) => Nullable(schema)
              )([A] => (value: Header.Schema.Object.Value.Nullable[A]) => value.self)

      given EnrichedSchemaInvariant[Header.Schema.Object] with
        override def imap[A, B](fa: Header.Schema.Object[A])(f: A => B)(g: B => A): Object[B] =
          fa match
            case Dictionary(self) => Dictionary(self.map(_.imap(f)(g)))
            case Record(self)     => Record(self.map(_.imap(f)(g)))

        extension [A](self: Object[A])
          override def metadata: Metadata = self match
            case Dictionary(self) => self.metadata
            case Record(self)     => self.metadata

          override def metadata(f: Metadata => Metadata): Object[A] = self match
            case Dictionary(self) => Dictionary(self.copy(metadata = f(self.metadata)))
            case Record(self)     => Record(self.copy(metadata = f(self.metadata)))

    final case class Field[A](self: Enrichment[Self.Field[Key, Header.Schema.Object.Value, A]])

    object Field:
      given EnrichedFieldSchemaInvariant[Header.Schema.Field, Key, Header.Schema.Object.Value] =
        EnrichedFieldSchemaInvariant[[a] =>> Enrichment[
          Self.Field[Key, Header.Schema.Object.Value, a]
        ], Key, Header.Schema.Object.Value]
          .imapK(
            [A] => (schema: Enrichment[Self.Field[Key, Header.Schema.Object.Value, A]]) => Field(schema)
          )([A] => (value: Header.Schema.Field[A]) => value.self)

    given EnrichedSchemaInvariant[Header.Schema] with
      override def imap[A, B](fa: Header.Schema[A])(f: A => B)(g: B => A): Header.Schema[B] = fa match
        case schema: Header.Schema.Value[A]  => schema.imap(f)(g)
        case schema: Header.Schema.Array[A]  => schema.imap(f)(g)
        case schema: Header.Schema.Object[A] => schema.imap(f)(g)

      extension [A](self: Header.Schema[A])
        override def metadata: Metadata = self match
          case schema: Header.Schema.Value[A]  => schema.metadata
          case schema: Header.Schema.Array[A]  => schema.metadata
          case schema: Header.Schema.Object[A] => schema.metadata

        override def metadata(f: Metadata => Metadata): Header.Schema[A] = self match
          case schema: Header.Schema.Value[A]  => schema.metadata(f)
          case schema: Header.Schema.Array[A]  => schema.metadata(f)
          case schema: Header.Schema.Object[A] => schema.metadata(f)

  enum Style:
    case Label
    case Matrix
    case Simple

  type Data = (CIString, String)

  given EnrichedSchemaInvariant[Header] with
    override def imap[A, B](fa: Header[A])(f: A => B)(g: B => A): Header[B] =
      fa.copy(self = fa.self.map(_.imap(f)(g)))

    extension [A](self: Header[A])
      def metadata: Metadata = self.self.metadata
      def metadata(f: Metadata => Metadata): Header[A] = self.copy(self = self.self.modifyMetadata(f))
