package io.taig.otter.http

import cats.Show
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.schema.*
import Self.Enriched
import Self.schema.ConstantSchema
import Self.Key

sealed abstract class Parameter[A] extends Product, Serializable:
  def name: String

  def style: Parameter.Style
  def modifyStyle(f: Parameter.Style => Parameter.Style): Parameter[A]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Parameter[A]

  final def imap[B](f: A => B)(g: B => A): Parameter[B] = Parameter.Modify(self = this, f, g)

  final def toPath: Path[A] = Path.Root(parameter = this)

  override def toString: String = this match
    case Parameter.Root(name, _, _, _) => s"{$name}"
    case Parameter.Modify(self, _, _)  => self.toString

object Parameter:
  final private[otter] case class Root[A](
      name: String,
      schema: Reference[Parameter.Value, A],
      style: Parameter.Style,
      metadata: Metadata
  ) extends Parameter[A]:
    override def modifyStyle(f: Style => Style): Parameter[A] = copy(style = f(style))
    override def modifyMetadata(f: Metadata => Metadata): Parameter[A] = copy(metadata = f(metadata))

  final private[otter] case class Modify[A, B](self: Parameter[A], f: A => B, g: B => A) extends Parameter[B]:
    export self.{metadata, name, style}
    override def modifyStyle(f: Style => Style): Parameter[B] = copy(self = self.modifyStyle(f))
    override def modifyMetadata(f: Metadata => Metadata): Parameter[B] = copy(self = self.modifyMetadata(f))

  sealed trait Value[A] extends Product with Serializable

  object Value:
    sealed trait Atom[A] extends Parameter.Value[A], Parameter.Value.Object.Atom[A]

    object Atom:
      final case class Constant[A](self: Enriched[Self.Constant[Parameter.Value.Atom.Primitive, *], A])
          extends Parameter.Value.Atom[A]

      object Constant:
        given ConstantSchema[Parameter.Value.Atom.Constant, Parameter.Value.Atom.Primitive] =
          ConstantSchema[Self.Constant[Parameter.Value.Atom.Primitive, *], Parameter.Value.Atom.Primitive]
            .imapK(
              [A] => (schema: Self.Constant[Parameter.Value.Atom.Primitive, A]) => Constant(Enriched(schema))
            )([A] => (value: Parameter.Value.Atom.Constant[A]) => value.self.self)

        given EnrichedSchema[Parameter.Value.Atom.Constant] =
          EnrichedSchema[Enriched[Self.Constant[Parameter.Value.Atom.Primitive, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Constant[Parameter.Value.Atom.Primitive, *], A]) => Constant(schema)
            )([A] => (value: Parameter.Value.Atom.Constant[A]) => value.self)

      final case class Enumeration[A](self: Enriched[Self.Enumeration[Parameter.Value.Atom.Primitive, *], A])
          extends Parameter.Value.Atom[A]

      object Enumeration:
        given EnumerationSchema[Parameter.Value.Atom.Enumeration, Parameter.Value.Atom.Primitive] =
          EnumerationSchema[Self.Enumeration[Parameter.Value.Atom.Primitive, *], Parameter.Value.Atom.Primitive]
            .imapK(
              [A] => (schema: Self.Enumeration[Parameter.Value.Atom.Primitive, A]) => Enumeration(Enriched(schema))
            )([A] => (value: Parameter.Value.Atom.Enumeration[A]) => value.self.self)

        given EnrichedSchema[Parameter.Value.Atom.Enumeration] =
          EnrichedSchema[Enriched[Self.Enumeration[Parameter.Value.Atom.Primitive, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Enumeration[Parameter.Value.Atom.Primitive, *], A]) => Enumeration(schema)
            )([A] => (value: Parameter.Value.Atom.Enumeration[A]) => value.self)

      final case class Primitive[A](self: Enriched[Self.Primitive.String, A]) extends Parameter.Value.Atom[A]

      object Primitive:
        given PrimitiveSchema.String[Parameter.Value.Atom.Primitive] =
          PrimitiveSchema
            .String[Self.Primitive.String]
            .imapK(
              [A] => (schema: Self.Primitive.String[A]) => Primitive(Enriched(schema))
            )([A] => (value: Parameter.Value.Atom.Primitive[A]) => value.self.self)

        given EnrichedSchema[Parameter.Value.Atom.Primitive] =
          EnrichedSchema[Enriched[Self.Primitive.String, *]]
            .imapK(
              [A] => (schema: Enriched[Self.Primitive.String, A]) => Primitive(schema)
            )([A] => (value: Parameter.Value.Atom.Primitive[A]) => value.self)

      final case class Union[A](self: Enriched[Self.Union[Parameter.Value.Atom, *], A]) extends Parameter.Value.Atom[A]

      object Union:
        given UnionSchema[Parameter.Value.Atom.Union, Parameter.Value.Atom] =
          UnionSchema[Self.Union[Parameter.Value.Atom, *], Parameter.Value.Atom]
            .imapK(
              [A] => (schema: Self.Union[Parameter.Value.Atom, A]) => Union(Enriched(schema))
            )([A] => (value: Parameter.Value.Atom.Union[A]) => value.self.self)

        given EnrichedSchema[Parameter.Value.Atom.Union] =
          EnrichedSchema[Enriched[Self.Union[Parameter.Value.Atom, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Union[Parameter.Value.Atom, *], A]) => Union(schema)
            )([A] => (value: Parameter.Value.Atom.Union[A]) => value.self)

      given EnrichedSchema[Parameter.Value.Atom] with
        override def imap[A, B](fa: Atom[A])(f: A => B)(g: B => A): Parameter.Value.Atom[B] = fa match
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

    sealed trait Array[A] extends Parameter.Value[A]

    object Array:
      final case class Collection[A](self: Enriched[Self.Collection[Parameter.Value.Atom, *], A])
          extends Parameter.Value.Array[A]

      object Collection:
        given CollectionSchema[Parameter.Value.Array.Collection, Parameter.Value.Atom] =
          CollectionSchema[Self.Collection[Parameter.Value.Atom, *], Parameter.Value.Atom]
            .imapK(
              [A] => (schema: Self.Collection[Parameter.Value.Atom, A]) => Collection(Enriched(schema))
            )([A] => (value: Parameter.Value.Array.Collection[A]) => value.self.self)

        given EnrichedSchema[Parameter.Value.Array.Collection] =
          EnrichedSchema[Enriched[Self.Collection[Parameter.Value.Atom, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Collection[Parameter.Value.Atom, *], A]) => Collection(schema)
            )([A] => (value: Parameter.Value.Array.Collection[A]) => value.self)

      final case class Tuple[A](self: Enriched[Self.Tuple[Parameter.Value.Atom, *], A]) extends Parameter.Value.Array[A]

      object Tuple:
        given TupleSchema[Parameter.Value.Array.Tuple, Parameter.Value.Atom] =
          TupleSchema[Self.Tuple[Parameter.Value.Atom, *], Parameter.Value.Atom]
            .imapK(
              [A] => (schema: Self.Tuple[Parameter.Value.Atom, A]) => Tuple(Enriched(schema))
            )([A] => (value: Parameter.Value.Array.Tuple[A]) => value.self.self)

        given EnrichedSchema[Parameter.Value.Array.Tuple] =
          EnrichedSchema[Enriched[Self.Tuple[Parameter.Value.Atom, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Tuple[Parameter.Value.Atom, *], A]) => Tuple(schema)
            )([A] => (value: Parameter.Value.Array.Tuple[A]) => value.self)

      given EnrichedSchema[Parameter.Value.Array] with
        override def imap[A, B](fa: Array[A])(f: A => B)(g: B => A): Parameter.Value.Array[B] = fa match
          case Collection(self) => Collection(self.mapF(_.imap(f)(g)))
          case Tuple(self)      => Tuple(self.mapF(_.imap(f)(g)))

        extension [A](self: Array[A])
          override def metadata: Metadata = self match
            case Collection(self) => self.metadata
            case Tuple(self)      => self.metadata

          override def metadata(f: Metadata => Metadata): Array[A] = self match
            case Collection(self) => Collection(self.copy(metadata = f(self.metadata)))
            case Tuple(self)      => Tuple(self.copy(metadata = f(self.metadata)))

    sealed trait Object[A] extends Parameter.Value[A]

    object Object:
      final case class Dictionary[A](self: Enriched[Self.Dictionary[Key, Parameter.Value.Atom, *], A])
          extends Parameter.Value.Object[A]

      object Dictionary:
        given DictionarySchema[Parameter.Value.Object.Dictionary, Key, Parameter.Value.Atom] =
          DictionarySchema[Self.Dictionary[Key, Parameter.Value.Atom, *], Key, Parameter.Value.Atom]
            .imapK(
              [A] => (schema: Self.Dictionary[Key, Parameter.Value.Atom, A]) => Dictionary(Enriched(schema))
            )([A] => (value: Parameter.Value.Object.Dictionary[A]) => value.self.self)

        given EnrichedSchema[Parameter.Value.Object.Dictionary] =
          EnrichedSchema[Enriched[Self.Dictionary[Key, Parameter.Value.Atom, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Dictionary[Key, Parameter.Value.Atom, *], A]) => Dictionary(schema)
            )([A] => (value: Parameter.Value.Object.Dictionary[A]) => value.self)

      final case class Record[A](self: Enriched[Self.Record[Parameter.Value.Field, *], A])
          extends Parameter.Value.Object[A]

      object Record:
        given RecordSchema[Parameter.Value.Object.Record, Parameter.Value.Field] =
          RecordSchema[Self.Record[Parameter.Value.Field, *], Parameter.Value.Field]
            .imapK(
              [A] => (schema: Self.Record[Parameter.Value.Field, A]) => Record(Enriched(schema))
            )([A] => (value: Parameter.Value.Object.Record[A]) => value.self.self)

        given EnrichedSchema[Parameter.Value.Object.Record] =
          EnrichedSchema[Enriched[Self.Record[Parameter.Value.Field, *], *]]
            .imapK(
              [A] => (schema: Enriched[Self.Record[Parameter.Value.Field, *], A]) => Record(schema)
            )([A] => (value: Parameter.Value.Object.Record[A]) => value.self)

      sealed trait Atom[A] extends Product with Serializable

      object Atom:
        final case class Nullable[A](self: Enriched[Self.Nullable[Parameter.Value.Object.Atom, *], A])
            extends Parameter.Value.Object.Atom[A]

        object Nullable:
          given NullableSchema[Parameter.Value.Object.Atom.Nullable, Parameter.Value.Object.Atom] =
            NullableSchema[Self.Nullable[Parameter.Value.Object.Atom, *], Parameter.Value.Object.Atom]
              .imapK(
                [A] => (schema: Self.Nullable[Parameter.Value.Object.Atom, A]) => Nullable(Enriched(schema))
              )([A] => (value: Parameter.Value.Object.Atom.Nullable[A]) => value.self.self)

          given EnrichedSchema[Parameter.Value.Object.Atom.Nullable] =
            EnrichedSchema[Enriched[Self.Nullable[Parameter.Value.Object.Atom, *], *]]
              .imapK(
                [A] => (schema: Enriched[Self.Nullable[Parameter.Value.Object.Atom, *], A]) => Nullable(schema)
              )([A] => (value: Parameter.Value.Object.Atom.Nullable[A]) => value.self)

      given EnrichedSchema[Parameter.Value.Object] with
        override def imap[A, B](fa: Parameter.Value.Object[A])(f: A => B)(g: B => A): Parameter.Value.Object[B] =
          fa match
            case Dictionary(self) => Dictionary(self.mapF(_.imap(f)(g)))
            case Record(self)     => Record(self.mapF(_.imap(f)(g)))

        extension [A](self: Object[A])
          override def metadata: Metadata = self match
            case Dictionary(self) => self.metadata
            case Record(self)     => self.metadata

          override def metadata(f: Metadata => Metadata): Parameter.Value.Object[A] = self match
            case Dictionary(self) => Dictionary(self.copy(metadata = f(self.metadata)))
            case Record(self)     => Record(self.copy(metadata = f(self.metadata)))

    final case class Field[A](self: Enriched[Self.Field[Key, Parameter.Value.Object.Atom, *], A])

    object Field:
      given FieldSchema[Parameter.Value.Field, Key, Parameter.Value.Object.Atom] =
        FieldSchema[
          Self.Field[Key, Parameter.Value.Object.Atom, *],
          Key,
          Parameter.Value.Object.Atom
        ]
          .imapK(
            [A] => (schema: Self.Field[Key, Parameter.Value.Object.Atom, A]) => Field(Enriched(schema))
          )([A] => (value: Parameter.Value.Field[A]) => value.self.self)

      given EnrichedSchema[Parameter.Value.Field] =
        EnrichedSchema[Enriched[Self.Field[Key, Parameter.Value.Object.Atom, *], *]]
          .imapK(
            [A] => (schema: Enriched[Self.Field[Key, Parameter.Value.Object.Atom, *], A]) => Field(schema)
          )([A] => (value: Parameter.Value.Field[A]) => value.self)

    given EnrichedSchema[Parameter.Value] with
      override def imap[A, B](fa: Parameter.Value[A])(f: A => B)(g: B => A): Parameter.Value[B] = fa match
        case schema: Parameter.Value.Atom[A]   => schema.imap(f)(g)
        case schema: Parameter.Value.Array[A]  => schema.imap(f)(g)
        case schema: Parameter.Value.Object[A] => schema.imap(f)(g)

      extension [A](self: Value[A])
        override def metadata: Metadata = self match
          case schema: Parameter.Value.Atom[A]   => schema.metadata
          case schema: Parameter.Value.Array[A]  => schema.metadata
          case schema: Parameter.Value.Object[A] => schema.metadata

        override def metadata(f: Metadata => Metadata): Value[A] = self match
          case schema: Parameter.Value.Atom[A]   => schema.metadata(f)
          case schema: Parameter.Value.Array[A]  => schema.metadata(f)
          case schema: Parameter.Value.Object[A] => schema.metadata(f)

  enum Style:
    case Simple, Label, Matrix

  given Schema[Parameter] with
    override def imap[A, B](fa: Parameter[A])(f: A => B)(g: B => A): Parameter[B] = fa.imap(f)(g)

  given [A]: Show[Parameter[A]] = Show.fromToString
