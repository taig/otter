package io.taig.otter.http

import io.taig.otter as Self
import io.taig.otter.*
import io.taig.otter.schema.Schema
import org.typelevel.ci.CIString
import io.taig.otter.schema.*
import cats.syntax.all.*

sealed abstract class Header[A] extends Product, Serializable:
  def name: CIString

  def schema: Reference[Header.Value, ?]

  def isOptional: Boolean

  final def imap[B](f: A => B)(g: B => A): Header[B] = Header.Modify(self = this, f, g)

  final def optional: Header[Option[A]] = Header.Optional(self = this)

  final def toHeaders: Headers[A] = Headers.Root(header = this)

object Header:
  final private[otter] case class Root[A](name: CIString, schema: Reference[Header.Value, A]) extends Header[A]:
    override def isOptional: Boolean = false

  final private[otter] case class Optional[A](self: Header[A]) extends Header[Option[A]]:
    export self.{name, schema}
    override def isOptional: Boolean = true

  final private[otter] case class Modify[A, B](self: Header[A], f: A => B, g: B => A) extends Header[B]:
    export self.{isOptional, name, schema}

  sealed trait Value[A] extends Product with Serializable

  object Value:
    sealed trait Atom[A] extends Header.Value[A], Header.Value.Object.Atom[A]

    object Atom:
      final case class Constant[A](self: Enrichment[Self.Constant[Header.Value.Atom.Primitive, *], A])
          extends Header.Value.Atom[A]

      object Constant:
        given ConstantSchema[Header.Value.Atom.Constant, Header.Value.Atom.Primitive] =
          ConstantSchema[Self.Constant[Header.Value.Atom.Primitive, *], Header.Value.Atom.Primitive]
            .imapK(
              [A] => (schema: Self.Constant[Header.Value.Atom.Primitive, A]) => Constant(Enrichment(schema))
            )([A] => (value: Header.Value.Atom.Constant[A]) => value.self.self)

        given EnrichedSchema[Header.Value.Atom.Constant] =
          EnrichedSchema[Enrichment[Self.Constant[Header.Value.Atom.Primitive, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Constant[Header.Value.Atom.Primitive, *], A]) => Constant(schema)
            )([A] => (value: Header.Value.Atom.Constant[A]) => value.self)

      final case class Enumeration[A](self: Enrichment[Self.Enumeration[Header.Value.Atom.Primitive, *], A])
          extends Header.Value.Atom[A]

      object Enumeration:
        given EnumerationSchema[Header.Value.Atom.Enumeration, Header.Value.Atom.Primitive] =
          EnumerationSchema[Self.Enumeration[Header.Value.Atom.Primitive, *], Header.Value.Atom.Primitive]
            .imapK(
              [A] => (schema: Self.Enumeration[Header.Value.Atom.Primitive, A]) => Enumeration(Enrichment(schema))
            )([A] => (value: Header.Value.Atom.Enumeration[A]) => value.self.self)

        given EnrichedSchema[Header.Value.Atom.Enumeration] =
          EnrichedSchema[Enrichment[Self.Enumeration[Header.Value.Atom.Primitive, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Enumeration[Header.Value.Atom.Primitive, *], A]) => Enumeration(schema)
            )([A] => (value: Header.Value.Atom.Enumeration[A]) => value.self)

      final case class Primitive[A](self: Enrichment[Self.Primitive.String, A]) extends Header.Value.Atom[A]

      object Primitive:
        given PrimitiveSchema.String[Header.Value.Atom.Primitive] = PrimitiveSchema
          .String[Self.Primitive.String]
          .imapK(
            [A] => (schema: Self.Primitive.String[A]) => Primitive(Enrichment(schema))
          )([A] => (value: Header.Value.Atom.Primitive[A]) => value.self.self)

        given EnrichedSchema[Header.Value.Atom.Primitive] = EnrichedSchema[Enrichment[Self.Primitive.String, *]]
          .imapK(
            [A] => (schema: Enriched[Self.Primitive.String, A]) => Primitive(schema)
          )([A] => (value: Header.Value.Atom.Primitive[A]) => value.self)

      final case class Union[A](self: Enrichment[Self.Union[Header.Value.Atom, *], A]) extends Header.Value.Atom[A]

      object Union:
        given UnionSchema[Header.Value.Atom.Union, Header.Value.Atom] =
          UnionSchema[Self.Union[Header.Value.Atom, *], Header.Value.Atom]
            .imapK(
              [A] => (schema: Self.Union[Header.Value.Atom, A]) => Union(Enrichment(schema))
            )([A] => (value: Header.Value.Atom.Union[A]) => value.self.self)

        given EnrichedSchema[Header.Value.Atom.Union] = EnrichedSchema[Enrichment[Self.Union[Header.Value.Atom, *], *]]
          .imapK(
            [A] => (schema: Enriched[Self.Union[Header.Value.Atom, *], A]) => Union(schema)
          )([A] => (value: Header.Value.Atom.Union[A]) => value.self)

      given EnrichedSchema[Header.Value.Atom] with
        override def imap[A, B](fa: Header.Value.Atom[A])(f: A => B)(g: B => A): Header.Value.Atom[B] = fa match
          case Constant(self)    => Constant(self.mapF(_.imap(f)(g)))
          case Enumeration(self) => Enumeration(self.mapF(_.imap(f)(g)))
          case Primitive(self)   => Primitive(self.mapF(_.imap(f)(g)))
          case Union(self)       => Union(self.mapF(_.imap(f)(g)))

        extension [A](self: Atom[A])
          override def metadata: Metadata = self match
            case Constant(self)    => self.metadata
            case Enumeration(self) => self.metadata
            case Primitive(self)   => self.metadata
            case Union(self)       => self.metadata
          override def metadata(f: Metadata => Metadata): Atom[A] = self match
            case Constant(self)    => Constant(self.copy(metadata = f(self.metadata)))
            case Enumeration(self) => Enumeration(self.copy(metadata = f(self.metadata)))
            case Primitive(self)   => Primitive(self.copy(metadata = f(self.metadata)))
            case Union(self)       => Union(self.copy(metadata = f(self.metadata)))

    sealed trait Array[A] extends Header.Value[A]

    object Array:
      final case class Collection[A](self: Enrichment[Self.Collection[Header.Value.Atom, *], A])
          extends Header.Value.Array[A]

      object Collection:
        given CollectionSchema[Header.Value.Array.Collection, Header.Value.Atom] =
          CollectionSchema[Self.Collection[Header.Value.Atom, *], Header.Value.Atom]
            .imapK(
              [A] => (schema: Self.Collection[Header.Value.Atom, A]) => Collection(Enrichment(schema))
            )([A] => (value: Header.Value.Array.Collection[A]) => value.self.self)

        given EnrichedSchema[Header.Value.Array.Collection] =
          EnrichedSchema[Enrichment[Self.Collection[Header.Value.Atom, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Collection[Header.Value.Atom, *], A]) => Collection(schema)
            )([A] => (value: Header.Value.Array.Collection[A]) => value.self)

      final case class Tuple[A](self: Enrichment[Self.Tuple[Header.Value.Atom, *], A]) extends Header.Value.Array[A]

      object Tuple:
        given TupleSchema[Header.Value.Array.Tuple, Header.Value.Atom] =
          TupleSchema[Self.Tuple[Header.Value.Atom, *], Header.Value.Atom]
            .imapK(
              [A] => (schema: Self.Tuple[Header.Value.Atom, A]) => Tuple(Enrichment(schema))
            )([A] => (value: Header.Value.Array.Tuple[A]) => value.self.self)

        given EnrichedSchema[Header.Value.Array.Tuple] =
          EnrichedSchema[Enrichment[Self.Tuple[Header.Value.Atom, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Tuple[Header.Value.Atom, *], A]) => Tuple(schema)
            )([A] => (value: Header.Value.Array.Tuple[A]) => value.self)

      given EnrichedSchema[Header.Value.Array] with
        override def imap[A, B](fa: Header.Value.Array[A])(f: A => B)(g: B => A): Header.Value.Array[B] = fa match
          case Collection(self) => Collection(self.mapF(_.imap(f)(g)))
          case Tuple(self)      => Tuple(self.mapF(_.imap(f)(g)))

        extension [A](self: Array[A])
          override def metadata: Metadata = self match
            case Collection(self) => self.metadata
            case Tuple(self)      => self.metadata

          override def metadata(f: Metadata => Metadata): Array[A] = self match
            case Collection(self) => Collection(self.copy(metadata = f(self.metadata)))
            case Tuple(self)      => Tuple(self.copy(metadata = f(self.metadata)))

    sealed trait Object[A] extends Header.Value[A]

    object Object:
      final case class Dictionary[A](self: Enrichment[Self.Dictionary[Key, Header.Value.Object.Atom, *], A])
          extends Header.Value.Object[A]

      object Dictionary:
        given DictionarySchema[Header.Value.Object.Dictionary, Key, Header.Value.Atom] =
          DictionarySchema[Self.Dictionary[Key, Header.Value.Object.Atom, *], Key, Header.Value.Atom]
            .imapK(
              [A] => (schema: Self.Dictionary[Key, Header.Value.Object.Atom, A]) => Dictionary(Enrichment(schema))
            )([A] => (value: Header.Value.Object.Dictionary[A]) => value.self.self)

        given EnrichedSchema[Header.Value.Object.Dictionary] =
          EnrichedSchema[Enrichment[Self.Dictionary[Key, Header.Value.Object.Atom, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Dictionary[Key, Header.Value.Object.Atom, *], A]) => Dictionary(schema)
            )([A] => (value: Header.Value.Object.Dictionary[A]) => value.self)

      final case class Record[A](self: Enrichment[Self.Record[Header.Value.Field, *], A]) extends Header.Value.Object[A]

      object Record:
        given RecordSchema[Header.Value.Object.Record, Header.Value.Field] =
          RecordSchema[Self.Record[Header.Value.Field, *], Header.Value.Field]
            .imapK(
              [A] => (schema: Self.Record[Header.Value.Field, A]) => Record(Enrichment(schema))
            )([A] => (value: Header.Value.Object.Record[A]) => value.self.self)

        given EnrichedSchema[Header.Value.Object.Record] =
          EnrichedSchema[Enrichment[Self.Record[Header.Value.Field, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Record[Header.Value.Field, *], A]) => Record(schema)
            )([A] => (value: Header.Value.Object.Record[A]) => value.self)

      sealed trait Atom[A] extends Product with Serializable

      object Atom:
        final case class Nullable[A](self: Enrichment[Self.Nullable[Header.Value.Object.Atom, *], A])
            extends Header.Value.Object.Atom[A]

        object Nullable:
          given NullableSchema[Header.Value.Object.Atom.Nullable, Header.Value.Object.Atom] =
            NullableSchema[Self.Nullable[Header.Value.Object.Atom, *], Header.Value.Object.Atom]
              .imapK(
                [A] => (schema: Self.Nullable[Header.Value.Object.Atom, A]) => Nullable(Enrichment(schema))
              )([A] => (value: Header.Value.Object.Atom.Nullable[A]) => value.self.self)

          given EnrichedSchema[Header.Value.Object.Atom.Nullable] =
            EnrichedSchema[Enrichment[Self.Nullable[Header.Value.Object.Atom, *], *]]
              .imapK(
                [A] => (schema: Enriched[Self.Nullable[Header.Value.Object.Atom, *], A]) => Nullable(schema)
              )([A] => (value: Header.Value.Object.Atom.Nullable[A]) => value.self)

      given EnrichedSchema[Header.Value.Object] with
        override def imap[A, B](fa: Header.Value.Object[A])(f: A => B)(g: B => A): Object[B] =
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

    final case class Field[A](self: Enrichment[Self.Field[Key, Header.Value.Object.Atom, *], A])

    object Field:
      given FieldSchema[Header.Value.Field, Key, Header.Value.Object.Atom] =
        FieldSchema[
          Self.Field[Key, Header.Value.Object.Atom, *],
          Key,
          Header.Value.Object.Atom
        ]
          .imapK(
            [A] => (schema: Self.Field[Key, Header.Value.Object.Atom, A]) => Field(Enrichment(schema))
          )([A] => (value: Header.Value.Field[A]) => value.self.self)

      given EnrichedSchema[Header.Value.Field] =
        EnrichedSchema[Enrichment[Self.Field[Key, Header.Value.Object.Atom, *], *]]
          .imapK(
            [A] => (schema: Enriched[Self.Field[Key, Header.Value.Object.Atom, *], A]) => Field(schema)
          )([A] => (value: Header.Value.Field[A]) => value.self)

    given EnrichedSchema[Header.Value] with
      override def imap[A, B](fa: Header.Value[A])(f: A => B)(g: B => A): Header.Value[B] = fa match
        case schema: Header.Value.Atom[A]   => schema.imap(f)(g)
        case schema: Header.Value.Array[A]  => schema.imap(f)(g)
        case schema: Header.Value.Object[A] => schema.imap(f)(g)

      extension [A](self: Header.Value[A])
        override def metadata: Metadata = self match
          case schema: Header.Value.Atom[A]   => schema.metadata
          case schema: Header.Value.Array[A]  => schema.metadata
          case schema: Header.Value.Object[A] => schema.metadata

        override def metadata(f: Metadata => Metadata): Header.Value[A] = self match
          case schema: Header.Value.Atom[A]   => schema.metadata(f)
          case schema: Header.Value.Array[A]  => schema.metadata(f)
          case schema: Header.Value.Object[A] => schema.metadata(f)

  enum Style:
    case Label
    case Matrix
    case Simple

  type Data = (CIString, String)

  given Schema[Header] with
    override def imap[A, B](fa: Header[A])(f: A => B)(g: B => A): Header[B] = fa.imap(f)(g)
