package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.*
import io.taig.otter.operation.*
import io.taig.otter.syntax.EnrichedSyntax.*
import org.typelevel.ci.CIString

final case class Header[A](value: Header.Value[A], metadata: Metadata):
  def name: CIString = value.name
  def schema: Header.Schema[?] = value.schema.value
  def isOptional: Boolean = value.isOptional
  def optional: Header[Option[A]] = Header(value = value.optional, metadata = Metadata.Empty)

  def :*[B](header: Header[B])(using merge: Merge[A, B]): Headers[merge.Out] = toHeaders :* header

  def *:[B](header: Header[B])(using merge: Merge[B, A]): Headers[merge.Out] =
    header.toHeaders * toHeaders

  def toHeaders: Headers[A] = Headers(value = Headers.Value.Root(this), metadata = Metadata.Empty)

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

  sealed trait Schema[A] extends Header.Schema.Any[A]

  object Schema:
    sealed trait Any[A] extends Product, Serializable

    object Any:
      final case class Boolean[A](self: Self.Primitive.Boolean[A]) extends Header.Schema.Any[A]

      object Boolean:
        given PrimitiveSchemaInvariant.Boolean[Header.Schema.Any.Boolean] =
          PrimitiveSchemaInvariant
            .Boolean[Self.Primitive.Boolean]
            .imapK(
              [A] => (schema: Self.Primitive.Boolean[A]) => Boolean(schema)
            )([A] => (value: Header.Schema.Any.Boolean[A]) => value.self)

      final case class Number[A](self: Self.Primitive.Number[A]) extends Header.Schema.Any[A]

      object Number:
        given PrimitiveSchemaInvariant.Number[Header.Schema.Any.Number] =
          PrimitiveSchemaInvariant
            .Number[Self.Primitive.Number]
            .imapK(
              [A] => (schema: Self.Primitive.Number[A]) => Number(schema)
            )([A] => (value: Header.Schema.Any.Number[A]) => value.self)

    sealed trait Value[A] extends Header.Schema[A], Header.Schema.Object.Value[A]

    object Value:
      final case class Constant[A](self: Self.Constant[Header.Schema.Value.String, A]) extends Header.Schema.Value[A]

      object Constant:
        given ConstantSchemaInvariant[Header.Schema.Value.Constant, Header.Schema.Value.String] =
          ConstantSchemaInvariant[
            Self.Constant[Header.Schema.Value.String, *],
            Header.Schema.Value.String
          ].imapK(
            [A] => (schema: Self.Constant[Header.Schema.Value.String, A]) => Constant(schema)
          )([A] => (value: Header.Schema.Value.Constant[A]) => value.self)

      final case class Enumeration[A](self: Self.Enumeration[Header.Schema.Value.String, A])
          extends Header.Schema.Value[A]

      object Enumeration:
        given EnumerationSchemaInvariant[Header.Schema.Value.Enumeration, Header.Schema.Value.String] =
          EnumerationSchemaInvariant[
            Self.Enumeration[Header.Schema.Value.String, *],
            Header.Schema.Value.String
          ].imapK(
            [A] => (schema: Self.Enumeration[Header.Schema.Value.String, A]) => Enumeration(schema)
          )([A] => (value: Header.Schema.Value.Enumeration[A]) => value.self)

      final case class String[A](self: Self.Primitive.String[A]) extends Header.Schema.Value[A]

      object String:
        given PrimitiveSchemaInvariant.String[Header.Schema.Value.String] = PrimitiveSchemaInvariant
          .String[Self.Primitive.String]
          .imapK(
            [A] => (schema: Self.Primitive.String[A]) => String(schema)
          )([A] => (value: Header.Schema.Value.String[A]) => value.self)

      final case class Union[A](self: Self.Union[Header.Schema.Value, A]) extends Header.Schema.Value[A]

      object Union:
        given UnionSchemaInvariant[Header.Schema.Value.Union, Header.Schema.Value] =
          UnionSchemaInvariant[Self.Union[Header.Schema.Value, *], Header.Schema.Value]
            .imapK(
              [A] => (schema: Self.Union[Header.Schema.Value, A]) => Union(schema)
            )([A] => (value: Header.Schema.Value.Union[A]) => value.self)

      given SchemaInvariant[Header.Schema.Value] with
        override def imap[A, B](fa: Header.Schema.Value[A])(f: A => B)(g: B => A): Header.Schema.Value[B] = fa match
          case Constant(self)    => Constant(self.imap(f)(g))
          case Enumeration(self) => Enumeration(self.imap(f)(g))
          case String(self)      => String(self.imap(f)(g))
          case Union(self)       => Union(self.imap(f)(g))

        override def enriched[A]: Enriched[Header.Schema.Value[A]] = new Enriched[Header.Schema.Value[A]]:
          override def metadata(a: Header.Schema.Value[A]): Metadata = a match
            case Constant(self)    => self.metadata
            case Enumeration(self) => self.metadata
            case String(self)      => self.metadata
            case Union(self)       => self.metadata

          override def modifyMetadata(a: Header.Schema.Value[A])(f: Metadata => Metadata): Header.Schema.Value[A] =
            a match
              case Constant(self)    => Constant(self.metadata(f))
              case Enumeration(self) => Enumeration(self.metadata(f))
              case String(self)      => String(self.metadata(f))
              case Union(self)       => Union(self.metadata(f))

    sealed trait Array[A] extends Header.Schema[A]

    object Array:
      final case class Collection[A](self: Self.Collection[Header.Schema.Value, A]) extends Header.Schema.Array[A]

      object Collection:
        given CollectionSchemaInvariant[Header.Schema.Array.Collection, Header.Schema.Value] =
          CollectionSchemaInvariant[Self.Collection[Header.Schema.Value, *], Header.Schema.Value]
            .imapK(
              [A] => (schema: Self.Collection[Header.Schema.Value, A]) => Collection(schema)
            )([A] => (value: Header.Schema.Array.Collection[A]) => value.self)

      final case class Tuple[A](self: Self.Tuple[Header.Schema.Value, A]) extends Header.Schema.Array[A]

      object Tuple:
        given TupleSchemaInvariant[Header.Schema.Array.Tuple, Header.Schema.Value] =
          TupleSchemaInvariant[Self.Tuple[Header.Schema.Value, *], Header.Schema.Value]
            .imapK(
              [A] => (schema: Self.Tuple[Header.Schema.Value, A]) => Tuple(schema)
            )([A] => (value: Header.Schema.Array.Tuple[A]) => value.self)

      given SchemaInvariant[Header.Schema.Array] with
        override def imap[A, B](fa: Header.Schema.Array[A])(f: A => B)(g: B => A): Header.Schema.Array[B] = fa match
          case Collection(self) => Collection(self.imap(f)(g))
          case Tuple(self)      => Tuple(self.imap(f)(g))

        override def enriched[A]: Enriched[Header.Schema.Array[A]] = new Enriched[Header.Schema.Array[A]]:
          override def metadata(a: Header.Schema.Array[A]): Metadata = a match
            case Collection(self) => self.metadata
            case Tuple(self)      => self.metadata

          override def modifyMetadata(a: Header.Schema.Array[A])(f: Metadata => Metadata): Header.Schema.Array[A] =
            a match
              case Collection(self) => Collection(self.metadata(f))
              case Tuple(self)      => Tuple(self.metadata(f))

    sealed trait Object[A] extends Header.Schema[A]

    object Object:
      final case class Dictionary[A](self: Self.Dictionary[Key, Header.Schema.Object.Value, A])
          extends Header.Schema.Object[A]

      object Dictionary:
        given DictionarySchemaInvariant[Header.Schema.Object.Dictionary, Key, Header.Schema.Value] =
          DictionarySchemaInvariant[
            [a] =>> Self.Dictionary[Key, Header.Schema.Object.Value, a],
            Key,
            Header.Schema.Value
          ]
            .imapK(
              [A] => (schema: Self.Dictionary[Key, Header.Schema.Object.Value, A]) => Dictionary(schema)
            )([A] => (value: Header.Schema.Object.Dictionary[A]) => value.self)

      final case class Record[A](self: Self.Record[Header.Schema.Field, A]) extends Header.Schema.Object[A]

      object Record:
        given RecordSchemaInvariant[Header.Schema.Object.Record, Header.Schema.Field] =
          RecordSchemaInvariant[Self.Record[Header.Schema.Field, *], Header.Schema.Field]
            .imapK(
              [A] => (schema: Self.Record[Header.Schema.Field, A]) => Record(schema)
            )([A] => (value: Header.Schema.Object.Record[A]) => value.self)

      sealed trait Value[A] extends Product, Serializable

      object Value:
        final case class Nullable[A](self: Self.Nullable[Header.Schema.Object.Value, A])
            extends Header.Schema.Object.Value[A]

        object Nullable:
          given NullableSchemaInvariant[Header.Schema.Object.Value.Nullable, Header.Schema.Object.Value] =
            NullableSchemaInvariant[Self.Nullable[Header.Schema.Object.Value, *], Header.Schema.Object.Value]
              .imapK(
                [A] => (schema: Self.Nullable[Header.Schema.Object.Value, A]) => Nullable(schema)
              )([A] => (value: Header.Schema.Object.Value.Nullable[A]) => value.self)

      given SchemaInvariant[Header.Schema.Object] with
        override def imap[A, B](fa: Header.Schema.Object[A])(f: A => B)(g: B => A): Object[B] =
          fa match
            case Dictionary(self) => Dictionary(self.imap(f)(g))
            case Record(self)     => Record(self.imap(f)(g))

        override def enriched[A]: Enriched[Header.Schema.Object[A]] = new Enriched[Header.Schema.Object[A]]:
          override def metadata(a: Header.Schema.Object[A]): Metadata = a match
            case Dictionary(self) => self.metadata
            case Record(self)     => self.metadata

          override def modifyMetadata(a: Header.Schema.Object[A])(f: Metadata => Metadata): Header.Schema.Object[A] =
            a match
              case Dictionary(self) => Dictionary(self.metadata(f))
              case Record(self)     => Record(self.metadata(f))

    final case class Field[A](self: Self.Field[Key, Header.Schema.Object.Value, A])

    object Field:
      given FieldSchemaInvariant[Header.Schema.Field, Key, Header.Schema.Object.Value] =
        FieldSchemaInvariant[Self.Field[Key, Header.Schema.Object.Value, *], Key, Header.Schema.Object.Value]
          .imapK(
            [A] => (schema: Self.Field[Key, Header.Schema.Object.Value, A]) => Field(schema)
          )([A] => (value: Header.Schema.Field[A]) => value.self)

    given SchemaInvariant[Header.Schema] with
      override def imap[A, B](fa: Header.Schema[A])(f: A => B)(g: B => A): Header.Schema[B] = fa match
        case schema: Header.Schema.Value[A]  => schema.imap(f)(g)
        case schema: Header.Schema.Array[A]  => schema.imap(f)(g)
        case schema: Header.Schema.Object[A] => schema.imap(f)(g)

      override def enriched[A]: Enriched[Header.Schema[A]] = new Enriched[Header.Schema[A]]:
        override def metadata(a: Header.Schema[A]): Metadata = a match
          case schema: Header.Schema.Value[A]  => schema.metadata
          case schema: Header.Schema.Array[A]  => schema.metadata
          case schema: Header.Schema.Object[A] => schema.metadata

        override def modifyMetadata(a: Header.Schema[A])(f: Metadata => Metadata): Header.Schema[A] = a match
          case schema: Header.Schema.Value[A]  => schema.metadata(f)
          case schema: Header.Schema.Array[A]  => schema.metadata(f)
          case schema: Header.Schema.Object[A] => schema.metadata(f)

  enum Style:
    case Label
    case Matrix
    case Simple

  type Data = (CIString, String)

  given SchemaInvariant[Header] with
    override def imap[A, B](fa: Header[A])(f: A => B)(g: B => A): Header[B] =
      fa.copy(value = fa.value.imap(f)(g))

    override def enriched[A]: Enriched[Header[A]] = new Enriched[Header[A]]:
      override def metadata(a: Header[A]): Metadata = a.metadata
      override def modifyMetadata(a: Header[A])(f: Metadata => Metadata): Header[A] =
        a.copy(metadata = f(a.metadata))
