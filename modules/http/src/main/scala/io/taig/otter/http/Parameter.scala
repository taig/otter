package io.taig.otter.http

import cats.Show
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.Key
import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.operation.*
import io.taig.otter.syntax.EnrichedSyntax.*

final case class Parameter[A](value: Parameter.Value[A], metadata: Metadata):
  def name: String = value.name

  def schema: Parameter.Schema[?] = value.schema.value

  def style: Parameter.Style = value.style
  def style(f: Parameter.Style => Parameter.Style): Parameter[A] = copy(value = value.modifyStyle(f))
  def style(value: Parameter.Style): Parameter[A] = style(_ => value)

  def toPath: Path[A] = Path(value = Path.Value.Root(this), metadata = Metadata.Empty)

object Parameter:
  sealed abstract class Value[A] extends Product, Serializable:
    def name: String

    def schema: Reference[Parameter.Schema, ?]

    def style: Parameter.Style
    def modifyStyle(f: Parameter.Style => Parameter.Style): Parameter.Value[A]

    final def imap[B](f: A => B)(g: B => A): Parameter.Value[B] = Value.Modify(self = this, f, g)

    final override def toString: String = this match
      case Parameter.Value.Root(name, _, _)   => s"{$name}"
      case Parameter.Value.Modify(self, _, _) => self.toString

  object Value:
    final private[otter] case class Root[A](
        name: String,
        schema: Reference[Parameter.Schema, A],
        style: Parameter.Style
    ) extends Parameter.Value[A]:
      override def modifyStyle(f: Style => Style): Parameter.Value[A] = copy(style = f(style))

    final private[otter] case class Modify[A, B](self: Parameter.Value[A], f: A => B, g: B => A)
        extends Parameter.Value[B]:
      export self.{name, schema, style}
      override def modifyStyle(f: Style => Style): Parameter.Value[B] = copy(self = self.modifyStyle(f))

    given [A]: Show[Parameter.Value[A]] = Show.fromToString

  sealed trait Schema[A] extends Parameter.Schema.Any[A]

  object Schema:
    sealed trait Any[A] extends Product, Serializable

    sealed trait Primitive[A] extends Parameter.Schema.Any[A]:
      def self: Self.Primitive[Parameter.Schema.Primitive, A]

    object Primitive:
      final case class Boolean[A](self: Self.Primitive.Boolean[A]) extends Parameter.Schema.Primitive[A]

      object Boolean:
        given PrimitiveSchemaInvariant.Boolean[Parameter.Schema.Primitive.Boolean] =
          PrimitiveSchemaInvariant
            .Boolean[Self.Primitive.Boolean]
            .imapK(
              [A] => (schema: Self.Primitive.Boolean[A]) => Boolean(schema)
            )([A] => (value: Parameter.Schema.Primitive.Boolean[A]) => value.self)

      final case class Number[A](self: Self.Primitive.Number[A]) extends Parameter.Schema.Primitive[A]

      object Number:
        given PrimitiveSchemaInvariant.Number[Parameter.Schema.Primitive.Number] =
          PrimitiveSchemaInvariant
            .Number[Self.Primitive.Number]
            .imapK(
              [A] => (schema: Self.Primitive.Number[A]) => Number(schema)
            )([A] => (value: Parameter.Schema.Primitive.Number[A]) => value.self)

      final case class String[A](self: Self.Primitive.String[Parameter.Schema.Primitive, A])
          extends Parameter.Schema.Primitive[A],
            Parameter.Schema.Value[A]

      object String:
        given PrimitiveSchemaInvariant.String[Parameter.Schema.Primitive.String, Parameter.Schema.Primitive] =
          PrimitiveSchemaInvariant
            .String[Self.Primitive.String[Parameter.Schema.Primitive, *], Parameter.Schema.Primitive]
            .imapK(
              [A] => (schema: Self.Primitive.String[Parameter.Schema.Primitive, A]) => String(schema)
            )([A] => (value: Parameter.Schema.Primitive.String[A]) => value.self)

      given PrimitiveSchemaInvariant[Parameter.Schema.Primitive, Parameter.Schema.Primitive] =
        PrimitiveSchemaInvariant[Self.Primitive[Parameter.Schema.Primitive, *], Parameter.Schema.Primitive].imapK(
          [A] =>
            (schema: Self.Primitive[Parameter.Schema.Primitive, A]) =>
              schema match
                case self: Self.Primitive.Boolean[A] => Parameter.Schema.Primitive.Boolean(self)
                case self: Self.Primitive.Number[A]  => Parameter.Schema.Primitive.Number(self)
                case self: Self.Primitive.String[Parameter.Schema.Primitive, A] =>
                  Parameter.Schema.Primitive.String(self)
        )([A] => (value: Parameter.Schema.Primitive[A]) => value.self)

    sealed trait Value[A] extends Parameter.Schema[A], Parameter.Schema.Object.Value[A]

    object Value:
      final case class Constant[A](self: Self.Constant[Parameter.Schema.Primitive.String, A])
          extends Parameter.Schema.Value[A]

      object Constant:
        given ConstantSchemaInvariant[Parameter.Schema.Value.Constant, Parameter.Schema.Primitive.String] =
          ConstantSchemaInvariant[
            Self.Constant[Parameter.Schema.Primitive.String, *],
            Parameter.Schema.Primitive.String
          ].imapK(
            [A] => (schema: Self.Constant[Parameter.Schema.Primitive.String, A]) => Constant(schema)
          )([A] => (value: Parameter.Schema.Value.Constant[A]) => value.self)

      final case class Enumeration[A](self: Self.Enumeration[Parameter.Schema.Primitive.String, A])
          extends Parameter.Schema.Value[A]

      object Enumeration:
        given EnumerationSchemaInvariant[Parameter.Schema.Value.Enumeration, Parameter.Schema.Primitive.String] =
          EnumerationSchemaInvariant[
            Self.Enumeration[Parameter.Schema.Primitive.String, *],
            Parameter.Schema.Primitive.String
          ].imapK(
            [A] => (schema: Self.Enumeration[Parameter.Schema.Primitive.String, A]) => Enumeration(schema)
          )([A] => (value: Parameter.Schema.Value.Enumeration[A]) => value.self)

      final case class Union[A](self: Self.Union[Parameter.Schema.Value, A]) extends Parameter.Schema.Value[A]

      object Union:
        given UnionSchemaInvariant[Parameter.Schema.Value.Union, Parameter.Schema.Value] =
          UnionSchemaInvariant[
            Self.Union[Parameter.Schema.Value, *],
            Parameter.Schema.Value
          ].imapK(
            [A] => (schema: Self.Union[Parameter.Schema.Value, A]) => Union(schema)
          )([A] => (value: Parameter.Schema.Value.Union[A]) => value.self)

      given SchemaInvariant[Parameter.Schema.Value] with
        override def imap[A, B](fa: Value[A])(f: A => B)(g: B => A): Parameter.Schema.Value[B] = fa match
          case Parameter.Schema.Primitive.String(self)  => Parameter.Schema.Primitive.String(self.imap(f)(g))
          case Parameter.Schema.Value.Constant(self)    => Constant(self.imap(f)(g))
          case Parameter.Schema.Value.Enumeration(self) => Enumeration(self.imap(f)(g))
          case Parameter.Schema.Value.Union(self)       => Union(self.imap(f)(g))

        override def enriched[A]: Enriched[Parameter.Schema.Value[A]] = new Enriched[Parameter.Schema.Value[A]]:
          override def metadata(a: Parameter.Schema.Value[A]): Metadata = a match
            case Parameter.Schema.Primitive.String(self)  => self.metadata
            case Parameter.Schema.Value.Constant(self)    => self.metadata
            case Parameter.Schema.Value.Enumeration(self) => self.metadata
            case Parameter.Schema.Value.Union(self)       => self.metadata

          override def modifyMetadata(a: Parameter.Schema.Value[A])(
              f: Metadata => Metadata
          ): Parameter.Schema.Value[A] = a match
            case Parameter.Schema.Primitive.String(self)  => Parameter.Schema.Primitive.String(self.metadata(f))
            case Parameter.Schema.Value.Constant(self)    => Constant(self.metadata(f))
            case Parameter.Schema.Value.Enumeration(self) => Enumeration(self.metadata(f))
            case Parameter.Schema.Value.Union(self)       => Union(self.metadata(f))

    sealed trait Array[A] extends Parameter.Schema[A]

    object Array:
      final case class Collection[A](self: Self.Collection[Parameter.Schema.Value, A]) extends Parameter.Schema.Array[A]

      object Collection:
        given CollectionSchemaInvariant[Parameter.Schema.Array.Collection, Parameter.Schema.Value] =
          CollectionSchemaInvariant[
            Self.Collection[Parameter.Schema.Value, *],
            Parameter.Schema.Value
          ].imapK(
            [A] => (schema: Self.Collection[Parameter.Schema.Value, A]) => Collection(schema)
          )([A] => (value: Parameter.Schema.Array.Collection[A]) => value.self)

      final case class Tuple[A](self: Self.Tuple[Parameter.Schema.Value, A]) extends Parameter.Schema.Array[A]

      object Tuple:
        given TupleSchemaInvariant[Parameter.Schema.Array.Tuple, Parameter.Schema.Value] =
          TupleSchemaInvariant[
            Self.Tuple[Parameter.Schema.Value, *],
            Parameter.Schema.Value
          ].imapK(
            [A] => (schema: Self.Tuple[Parameter.Schema.Value, A]) => Tuple(schema)
          )([A] => (value: Parameter.Schema.Array.Tuple[A]) => value.self)

      given SchemaInvariant[Parameter.Schema.Array] with
        override def imap[A, B](fa: Array[A])(f: A => B)(g: B => A): Parameter.Schema.Array[B] = fa match
          case Collection(self) => Collection(self.imap(f)(g))
          case Tuple(self)      => Tuple(self.imap(f)(g))

        override def enriched[A]: Enriched[Parameter.Schema.Array[A]] = new Enriched[Parameter.Schema.Array[A]]:
          override def metadata(a: Parameter.Schema.Array[A]): Metadata = a match
            case Collection(self) => self.metadata
            case Tuple(self)      => self.metadata

          override def modifyMetadata(a: Parameter.Schema.Array[A])(
              f: Metadata => Metadata
          ): Parameter.Schema.Array[A] =
            a match
              case Collection(self) => Collection(self.metadata(f))
              case Tuple(self)      => Tuple(self.metadata(f))

    sealed trait Object[A] extends Parameter.Schema[A]

    object Object:
      final case class Dictionary[A](self: Self.Dictionary[Key, Parameter.Schema.Value, A])
          extends Parameter.Schema.Object[A]

      object Dictionary:
        given DictionarySchemaInvariant[Parameter.Schema.Object.Dictionary, Key, Parameter.Schema.Value] =
          DictionarySchemaInvariant[
            Self.Dictionary[Key, Parameter.Schema.Value, *],
            Key,
            Parameter.Schema.Value
          ].imapK(
            [A] => (schema: Self.Dictionary[Key, Parameter.Schema.Value, A]) => Dictionary(schema)
          )([A] => (value: Parameter.Schema.Object.Dictionary[A]) => value.self)

      final case class Record[A](self: Self.Record[Parameter.Schema.Field, A]) extends Parameter.Schema.Object[A]

      object Record:
        given RecordSchemaInvariant[Parameter.Schema.Object.Record, Parameter.Schema.Field] =
          RecordSchemaInvariant[
            Self.Record[Parameter.Schema.Field, *],
            Parameter.Schema.Field
          ].imapK(
            [A] => (schema: Self.Record[Parameter.Schema.Field, A]) => Record(schema)
          )([A] => (value: Parameter.Schema.Object.Record[A]) => value.self)

      sealed trait Value[A] extends Product, Serializable

      object Value:
        final case class Nullable[A](self: Self.Nullable[Parameter.Schema.Object.Value, A])
            extends Parameter.Schema.Object.Value[A]

        object Nullable:
          given NullableSchemaInvariant[Parameter.Schema.Object.Value.Nullable, Parameter.Schema.Object.Value] =
            NullableSchemaInvariant[
              Self.Nullable[Parameter.Schema.Object.Value, *],
              Parameter.Schema.Object.Value
            ].imapK(
              [A] => (schema: Self.Nullable[Parameter.Schema.Object.Value, A]) => Nullable(schema)
            )([A] => (value: Parameter.Schema.Object.Value.Nullable[A]) => value.self)

      given SchemaInvariant[Parameter.Schema.Object] with
        override def imap[A, B](fa: Parameter.Schema.Object[A])(f: A => B)(g: B => A): Parameter.Schema.Object[B] =
          fa match
            case Dictionary(self) => Dictionary(self.imap(f)(g))
            case Record(self)     => Record(self.imap(f)(g))

        override def enriched[A]: Enriched[Parameter.Schema.Object[A]] = new Enriched[Parameter.Schema.Object[A]]:
          override def metadata(a: Parameter.Schema.Object[A]): Metadata = a match
            case Dictionary(self) => self.metadata
            case Record(self)     => self.metadata

          override def modifyMetadata(a: Parameter.Schema.Object[A])(
              f: Metadata => Metadata
          ): Parameter.Schema.Object[A] =
            a match
              case Dictionary(self) => Dictionary(self.metadata(f))
              case Record(self)     => Record(self.metadata(f))

    final case class Field[A](self: Self.Field[Key, Parameter.Schema.Object.Value, A])

    object Field:
      given FieldSchemaInvariant[Parameter.Schema.Field, Key, Parameter.Schema.Object.Value] =
        FieldSchemaInvariant[
          Self.Field[Key, Parameter.Schema.Object.Value, *],
          Key,
          Parameter.Schema.Object.Value
        ].imapK(
          [A] => (schema: Self.Field[Key, Parameter.Schema.Object.Value, A]) => Field(schema)
        )([A] => (value: Parameter.Schema.Field[A]) => value.self)

    given SchemaInvariant[Parameter.Schema] with
      override def imap[A, B](fa: Parameter.Schema[A])(f: A => B)(g: B => A): Parameter.Schema[B] = fa match
        case schema: Parameter.Schema.Value[A]  => schema.imap(f)(g)
        case schema: Parameter.Schema.Array[A]  => schema.imap(f)(g)
        case schema: Parameter.Schema.Object[A] => schema.imap(f)(g)

      override def enriched[A]: Enriched[Parameter.Schema[A]] = new Enriched[Parameter.Schema[A]]:
        override def metadata(a: Parameter.Schema[A]): Metadata = a match
          case schema: Parameter.Schema.Value[A]  => schema.metadata
          case schema: Parameter.Schema.Array[A]  => schema.metadata
          case schema: Parameter.Schema.Object[A] => schema.metadata

        override def modifyMetadata(a: Parameter.Schema[A])(f: Metadata => Metadata): Parameter.Schema[A] =
          a match
            case schema: Parameter.Schema.Value[A]  => schema.metadata(f)
            case schema: Parameter.Schema.Array[A]  => schema.metadata(f)
            case schema: Parameter.Schema.Object[A] => schema.metadata(f)

  enum Style:
    case Simple, Label, Matrix

  given SchemaInvariant[Parameter] with
    override def imap[A, B](fa: Parameter[A])(f: A => B)(g: B => A): Parameter[B] =
      fa.copy(value = fa.value.imap(f)(g))

    override def enriched[A]: Enriched[Parameter[A]] = new Enriched[Parameter[A]]:
      override def metadata(a: Parameter[A]): Metadata = a.metadata
      override def modifyMetadata(a: Parameter[A])(f: Metadata => Metadata): Parameter[A] =
        a.copy(metadata = f(a.metadata))

  given [A]: Show[Parameter[A]] = _.value.show
