package io.taig.otter.http

import io.taig.otter as Self
import io.taig.otter.*
import org.typelevel.ci.CIString
import io.taig.otter.operation.*
import cats.syntax.all.*

type Header[A] = Enrichment[Header.Value, A]

object Header:
  sealed abstract class Value[A] extends Product, Serializable:
    def name: CIString

    def schema: Reference[Header.Schema, ?]

    def isOptional: Boolean

    final def imap[B](f: A => B)(g: B => A): Value[B] = Value.Modify(self = this, f, g)

    final def optional: Value[Option[A]] = Value.Optional(self = this)

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
    sealed trait Atom[A] extends Header.Schema[A], Header.Schema.Object.Atom[A]

    object Atom:
      final case class Constant[A](self: Enrichment[Self.Constant[Header.Schema.Atom.Primitive, *], A])
          extends Header.Schema.Atom[A]

      object Constant:
        given EnrichedConstantSchemaInvariant[Header.Schema.Atom.Constant, Header.Schema.Atom.Primitive] =
          EnrichedConstantSchemaInvariant[
            Enrichment[Self.Constant[Header.Schema.Atom.Primitive, *], *],
            Header.Schema.Atom.Primitive
          ].imapK(
            [A] => (schema: Enrichment[Self.Constant[Header.Schema.Atom.Primitive, *], A]) => Constant(schema)
          )([A] => (value: Header.Schema.Atom.Constant[A]) => value.self)

      final case class Enumeration[A](self: Enrichment[Self.Enumeration[Header.Schema.Atom.Primitive, *], A])
          extends Header.Schema.Atom[A]

      object Enumeration:
        given EnrichedEnumerationSchemaInvariant[Header.Schema.Atom.Enumeration, Header.Schema.Atom.Primitive] =
          EnrichedEnumerationSchemaInvariant[
            Enrichment[Self.Enumeration[Header.Schema.Atom.Primitive, *], *],
            Header.Schema.Atom.Primitive
          ].imapK(
            [A] => (schema: Enrichment[Self.Enumeration[Header.Schema.Atom.Primitive, *], A]) => Enumeration(schema)
          )([A] => (value: Header.Schema.Atom.Enumeration[A]) => value.self)

      final case class Primitive[A](self: Enrichment[Self.Primitive.String, A]) extends Header.Schema.Atom[A]

      object Primitive:
        given EnrichedPrimitiveSchemaInvariant.String[Header.Schema.Atom.Primitive] =
          EnrichedPrimitiveSchemaInvariant
            .String[Enrichment[Self.Primitive.String, *]]
            .imapK(
              [A] => (schema: Enrichment[Self.Primitive.String, A]) => Primitive(schema)
            )([A] => (value: Header.Schema.Atom.Primitive[A]) => value.self)

      final case class Union[A](self: Enrichment[Self.Union[Header.Schema.Atom, *], A]) extends Header.Schema.Atom[A]

      object Union:
        given EnrichedUnionSchemaInvariant[Header.Schema.Atom.Union, Header.Schema.Atom] =
          EnrichedUnionSchemaInvariant[Enrichment[Self.Union[Header.Schema.Atom, *], *], Header.Schema.Atom].imapK(
            [A] => (schema: Enrichment[Self.Union[Header.Schema.Atom, *], A]) => Union(schema)
          )([A] => (value: Header.Schema.Atom.Union[A]) => value.self)

      given EnrichedSchemaInvariant[Header.Schema.Atom] with
        override def imap[A, B](fa: Header.Schema.Atom[A])(f: A => B)(g: B => A): Header.Schema.Atom[B] = fa match
          case Constant(self)    => Constant(self.mapF(_.imap(f)(g)))
          case Enumeration(self) => Enumeration(self.mapF(_.imap(f)(g)))
          case Primitive(self)   => Primitive(self.mapF(_.imap(f)(g)))
          case Union(self)       => Union(self.mapF(_.imap(f)(g)))

        extension [A](self: Header.Schema.Atom[A])
          override def metadata: Metadata = self match
            case Constant(self)    => self.metadata
            case Enumeration(self) => self.metadata
            case Primitive(self)   => self.metadata
            case Union(self)       => self.metadata
          override def metadata(f: Metadata => Metadata): Header.Schema.Atom[A] = self match
            case Constant(self)    => Constant(self.copy(metadata = f(self.metadata)))
            case Enumeration(self) => Enumeration(self.copy(metadata = f(self.metadata)))
            case Primitive(self)   => Primitive(self.copy(metadata = f(self.metadata)))
            case Union(self)       => Union(self.copy(metadata = f(self.metadata)))

    sealed trait Array[A] extends Header.Schema[A]

    object Array:
      final case class Collection[A](self: Enrichment[Self.Collection[Header.Schema.Atom, *], A])
          extends Header.Schema.Array[A]

      object Collection:
        given EnrichedCollectionSchemaInvariant[Header.Schema.Array.Collection, Header.Schema.Atom] =
          EnrichedCollectionSchemaInvariant[Enrichment[Self.Collection[Header.Schema.Atom, *], *], Header.Schema.Atom]
            .imapK(
              [A] => (schema: Enrichment[Self.Collection[Header.Schema.Atom, *], A]) => Collection(schema)
            )([A] => (value: Header.Schema.Array.Collection[A]) => value.self)

      final case class Tuple[A](self: Enrichment[Self.Tuple[Header.Schema.Atom, *], A]) extends Header.Schema.Array[A]

      object Tuple:
        given EnrichedTupleSchemaInvariant[Header.Schema.Array.Tuple, Header.Schema.Atom] =
          EnrichedTupleSchemaInvariant[Enrichment[Self.Tuple[Header.Schema.Atom, *], *], Header.Schema.Atom].imapK(
            [A] => (schema: Enrichment[Self.Tuple[Header.Schema.Atom, *], A]) => Tuple(schema)
          )([A] => (value: Header.Schema.Array.Tuple[A]) => value.self)

      given EnrichedSchemaInvariant[Header.Schema.Array] with
        override def imap[A, B](fa: Header.Schema.Array[A])(f: A => B)(g: B => A): Header.Schema.Array[B] = fa match
          case Collection(self) => Collection(self.mapF(_.imap(f)(g)))
          case Tuple(self)      => Tuple(self.mapF(_.imap(f)(g)))

        extension [A](self: Array[A])
          override def metadata: Metadata = self match
            case Collection(self) => self.metadata
            case Tuple(self)      => self.metadata

          override def metadata(f: Metadata => Metadata): Array[A] = self match
            case Collection(self) => Collection(self.copy(metadata = f(self.metadata)))
            case Tuple(self)      => Tuple(self.copy(metadata = f(self.metadata)))

    sealed trait Object[A] extends Header.Schema[A]

    object Object:
      final case class Dictionary[A](self: Enrichment[Self.Dictionary[Key, Header.Schema.Object.Atom, *], A])
          extends Header.Schema.Object[A]

      object Dictionary:
        given EnrichedDictionarySchemaInvariant[Header.Schema.Object.Dictionary, Key, Header.Schema.Atom] =
          EnrichedDictionarySchemaInvariant[Enrichment[
            Self.Dictionary[Key, Header.Schema.Object.Atom, *],
            *
          ], Key, Header.Schema.Atom]
            .imapK(
              [A] => (schema: Enrichment[Self.Dictionary[Key, Header.Schema.Object.Atom, *], A]) => Dictionary(schema)
            )([A] => (value: Header.Schema.Object.Dictionary[A]) => value.self)

      final case class Record[A](self: Enrichment[Self.Record[Header.Schema.Field, *], A])
          extends Header.Schema.Object[A]

      object Record:
        given EnrichedRecordSchemaInvariant[Header.Schema.Object.Record, Header.Schema.Field] =
          EnrichedRecordSchemaInvariant[Enrichment[Self.Record[Header.Schema.Field, *], *], Header.Schema.Field].imapK(
            [A] => (schema: Enrichment[Self.Record[Header.Schema.Field, *], A]) => Record(schema)
          )([A] => (value: Header.Schema.Object.Record[A]) => value.self)

      sealed trait Atom[A] extends Product with Serializable

      object Atom:
        final case class Nullable[A](self: Enrichment[Self.Nullable[Header.Schema.Object.Atom, *], A])
            extends Header.Schema.Object.Atom[A]

        object Nullable:
          given EnrichedNullableSchemaInvariant[Header.Schema.Object.Atom.Nullable, Header.Schema.Object.Atom] =
            EnrichedNullableSchemaInvariant[Enrichment[
              Self.Nullable[Header.Schema.Object.Atom, *],
              *
            ], Header.Schema.Object.Atom]
              .imapK(
                [A] => (schema: Enrichment[Self.Nullable[Header.Schema.Object.Atom, *], A]) => Nullable(schema)
              )([A] => (value: Header.Schema.Object.Atom.Nullable[A]) => value.self)

      given EnrichedSchemaInvariant[Header.Schema.Object] with
        override def imap[A, B](fa: Header.Schema.Object[A])(f: A => B)(g: B => A): Object[B] =
          fa match
            case Dictionary(self) => Dictionary(self.mapF(_.imap(f)(g)))
            case Record(self)     => Record(self.mapF(_.imap(f)(g)))

        extension [A](self: Object[A])
          override def metadata: Metadata = self match
            case Dictionary(self) => self.metadata
            case Record(self)     => self.metadata

          override def metadata(f: Metadata => Metadata): Object[A] = self match
            case Dictionary(self) => Dictionary(self.copy(metadata = f(self.metadata)))
            case Record(self)     => Record(self.copy(metadata = f(self.metadata)))

    final case class Field[A](self: Enrichment[Self.Field[Key, Header.Schema.Object.Atom, *], A])

    object Field:
      given EnrichedFieldSchemaInvariant[Header.Schema.Field, Key, Header.Schema.Object.Atom] =
        EnrichedFieldSchemaInvariant[Enrichment[
          Self.Field[Key, Header.Schema.Object.Atom, *],
          *
        ], Key, Header.Schema.Object.Atom]
          .imapK(
            [A] => (schema: Enrichment[Self.Field[Key, Header.Schema.Object.Atom, *], A]) => Field(schema)
          )([A] => (value: Header.Schema.Field[A]) => value.self)

    given EnrichedSchemaInvariant[Header.Schema] with
      override def imap[A, B](fa: Header.Schema[A])(f: A => B)(g: B => A): Header.Schema[B] = fa match
        case schema: Header.Schema.Atom[A]   => schema.imap(f)(g)
        case schema: Header.Schema.Array[A]  => schema.imap(f)(g)
        case schema: Header.Schema.Object[A] => schema.imap(f)(g)

      extension [A](self: Header.Schema[A])
        override def metadata: Metadata = self match
          case schema: Header.Schema.Atom[A]   => schema.metadata
          case schema: Header.Schema.Array[A]  => schema.metadata
          case schema: Header.Schema.Object[A] => schema.metadata

        override def metadata(f: Metadata => Metadata): Header.Schema[A] = self match
          case schema: Header.Schema.Atom[A]   => schema.metadata(f)
          case schema: Header.Schema.Array[A]  => schema.metadata(f)
          case schema: Header.Schema.Object[A] => schema.metadata(f)

  enum Style:
    case Label
    case Matrix
    case Simple

  type Data = (CIString, String)

  extension [A](self: Header[A]) def name: CIString = self.self.name

  given EnrichedSchemaInvariant[Header] with
    override def imap[A, B](fa: Header[A])(f: A => B)(g: B => A): Header[B] =
      fa.mapF(_.imap(f)(g))

    extension [A](self: Header[A])
      def metadata: Metadata = self.metadata
      def metadata(f: Metadata => Metadata): Header[A] = self.modifyMetadata(f)
