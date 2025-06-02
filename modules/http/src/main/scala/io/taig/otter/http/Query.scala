package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.*
import io.taig.otter.operation.*
import io.taig.otter.syntax.EnrichedSyntax.*

final case class Query[A](value: Query.Value[A], metadata: Metadata):
  def name: String = value.name
  def schema: Query.Schema[?] = value.schema.value

  def explode: Boolean = value.explode
  def explode(f: Boolean => Boolean): Query[A] = copy(value = value.modifyExplode(f))
  def explode(value: Boolean): Query[A] = explode(_ => value)

  def style: Query.Style = value.style
  def style(f: Query.Style => Query.Style): Query[A] = copy(value = value.modifyStyle(f))
  def style(value: Query.Style): Query[A] = style(_ => value)

  def isOptional: Boolean = value.isOptional

  def &[B](query: Query[B])(using merge: Merge[A, B]): Queries[merge.Out] = toQueries & query

  def toQueries: Queries[A] = Queries(value = Queries.Value.Root(this), metadata = Metadata.Empty)

object Query:
  sealed abstract class Value[A]:
    def name: String

    def schema: Reference[Query.Schema, ?]

    def explode: Boolean
    def modifyExplode(f: Boolean => Boolean): Query.Value[A]

    def style: Query.Style
    def modifyStyle(f: Query.Style => Query.Style): Query.Value[A]

    def isOptional: Boolean

    final def imap[B](f: A => B)(g: B => A): Query.Value[B] = Query.Value.Modify(self = this, f, g)

  object Value:
    final private[otter] case class Modify[A, B](self: Query.Value[A], f: A => B, g: B => A) extends Query.Value[B]:
      export self.{explode, isOptional, name, schema, style}
      override def modifyExplode(f: Boolean => Boolean): Query.Value[B] = copy(self = self.modifyExplode(f))
      override def modifyStyle(f: Style => Style): Query.Value[B] = copy(self = self.modifyStyle(f))

    final private[otter] case class Optional[A](self: Query.Value[A]) extends Query.Value[Option[A]]:
      export self.{explode, name, schema, style}
      override def isOptional: Boolean = true
      override def modifyExplode(f: Boolean => Boolean): Query.Value[Option[A]] = copy(self = self.modifyExplode(f))
      override def modifyStyle(f: Style => Style): Query.Value[Option[A]] = copy(self = self.modifyStyle(f))

    final private[otter] case class Root[A](
        name: String,
        schema: Reference[Query.Schema, A],
        explode: Boolean,
        style: Query.Style
    ) extends Query.Value[A]:
      override def isOptional: Boolean = false
      override def modifyExplode(f: Boolean => Boolean): Query.Value[A] = copy(explode = f(explode))
      override def modifyStyle(f: Query.Style => Query.Style): Query.Value[A] = copy(style = f(style))

  sealed trait Schema[A] extends Query.Schema.Any[A]

  object Schema:
    sealed trait Any[A] extends Product, Serializable

    sealed trait Primitive[A] extends Query.Schema.Any[A]:
      def self: Self.Primitive[Query.Schema.Primitive, A]

    object Primitive:
      final case class Boolean[A](self: Self.Primitive.Boolean[A]) extends Query.Schema.Primitive[A]

      object Boolean:
        given PrimitiveSchemaInvariant.Boolean[Query.Schema.Primitive.Boolean] =
          PrimitiveSchemaInvariant
            .Boolean[Self.Primitive.Boolean]
            .imapK(
              [A] => (schema: Self.Primitive.Boolean[A]) => Boolean(schema)
            )([A] => (schema: Query.Schema.Primitive.Boolean[A]) => schema.self)

      final case class Number[A](self: Self.Primitive.Number[A]) extends Query.Schema.Primitive[A]

      object Number:
        given PrimitiveSchemaInvariant.Number[Query.Schema.Primitive.Number] =
          PrimitiveSchemaInvariant
            .Number[Self.Primitive.Number]
            .imapK(
              [A] => (schema: Self.Primitive.Number[A]) => Number(schema)
            )([A] => (schema: Query.Schema.Primitive.Number[A]) => schema.self)

      final case class String[A](self: Self.Primitive.String[Query.Schema.Primitive, A])
          extends Query.Schema.Primitive[A],
            Query.Schema.Value[A]

      object String:
        given PrimitiveSchemaInvariant.String[Query.Schema.Primitive.String, Query.Schema.Primitive] =
          PrimitiveSchemaInvariant
            .String[Self.Primitive.String[Query.Schema.Primitive, *], Query.Schema.Primitive]
            .imapK(
              [A] => (schema: Self.Primitive.String[Query.Schema.Primitive, A]) => String(schema)
            )([A] => (schema: Query.Schema.Primitive.String[A]) => schema.self)

      given PrimitiveSchemaInvariant[Query.Schema.Primitive, Query.Schema.Primitive] =
        PrimitiveSchemaInvariant[Self.Primitive[Query.Schema.Primitive, *], Query.Schema.Primitive].imapK(
          [A] =>
            (self: Self.Primitive[Query.Schema.Primitive, A]) =>
              self match
                case self: Self.Primitive.Boolean[A]                        => Query.Schema.Primitive.Boolean(self)
                case self: Self.Primitive.Number[A]                         => Query.Schema.Primitive.Number(self)
                case self: Self.Primitive.String[Query.Schema.Primitive, A] => Query.Schema.Primitive.String(self)
        )([A] => (schema: Query.Schema.Primitive[A]) => schema.self)

    sealed trait Value[A] extends Query.Schema[A]

    object Value:
      final case class Constant[A](self: Self.Constant[Query.Schema.Primitive.String, A]) extends Value[A]

      object Constant:
        given ConstantSchemaInvariant[Query.Schema.Value.Constant, Query.Schema.Primitive.String] =
          ConstantSchemaInvariant[Self.Constant[Query.Schema.Primitive.String, *], Query.Schema.Primitive.String]
            .imapK(
              [A] => (schema: Self.Constant[Query.Schema.Primitive.String, A]) => Constant(schema)
            )([A] => (schema: Query.Schema.Value.Constant[A]) => schema.self)

      final case class Enumeration[A](self: Self.Enumeration[Query.Schema.Primitive.String, A]) extends Value[A]

      object Enumeration:
        given EnumerationSchemaInvariant[Query.Schema.Value.Enumeration, Query.Schema.Primitive.String] =
          EnumerationSchemaInvariant[Self.Enumeration[Query.Schema.Primitive.String, *], Query.Schema.Primitive.String]
            .imapK(
              [A] => (schema: Self.Enumeration[Query.Schema.Primitive.String, A]) => Enumeration(schema)
            )([A] => (schema: Query.Schema.Value.Enumeration[A]) => schema.self)

      final case class Union[A](self: Self.Union[Query.Schema.Value, A]) extends Value[A]

      object Union:
        given UnionSchemaInvariant[Query.Schema.Value.Union, Query.Schema.Value] =
          UnionSchemaInvariant[Self.Union[Query.Schema.Value, *], Query.Schema.Value].imapK(
            [A] => (schema: Self.Union[Query.Schema.Value, A]) => Union(schema)
          )([A] => (schema: Query.Schema.Value.Union[A]) => schema.self)

      given SchemaInvariant[Query.Schema.Value] with
        override def imap[A, B](fa: Query.Schema.Value[A])(f: A => B)(g: B => A): Query.Schema.Value[B] = fa match
          case Query.Schema.Value.Constant(self)    => Constant(self.imap(f)(g))
          case Query.Schema.Value.Enumeration(self) => Enumeration(self.imap(f)(g))
          case Query.Schema.Primitive.String(self)  => Query.Schema.Primitive.String(self.imap(f)(g))
          case Query.Schema.Value.Union(self)       => Union(self.imap(f)(g))

        override def enriched[A]: Enriched[Query.Schema.Value[A]] = new Enriched[Query.Schema.Value[A]]:
          override def metadata(a: Query.Schema.Value[A]): Metadata = a match
            case Query.Schema.Value.Constant(self)    => self.metadata
            case Query.Schema.Value.Enumeration(self) => self.metadata
            case Query.Schema.Primitive.String(self)  => self.metadata
            case Query.Schema.Value.Union(self)       => self.metadata

          override def modifyMetadata(a: Query.Schema.Value[A])(f: Metadata => Metadata): Query.Schema.Value[A] =
            a match
              case Query.Schema.Value.Constant(self)    => Constant(self.metadata(f))
              case Query.Schema.Value.Enumeration(self) => Enumeration(self.metadata(f))
              case Query.Schema.Primitive.String(self)  => Query.Schema.Primitive.String(self.metadata(f))
              case Query.Schema.Value.Union(self)       => Union(self.metadata(f))

    sealed trait Array[A] extends Query.Schema[A]

    object Array:
      final case class Collection[A](self: Self.Collection[Query.Schema.Value, A]) extends Query.Schema.Array[A]

      object Collection:
        given CollectionSchemaInvariant[Query.Schema.Array.Collection, Query.Schema.Value] =
          CollectionSchemaInvariant[Self.Collection[Query.Schema.Value, *], Query.Schema.Value].imapK(
            [A] => (schema: Self.Collection[Query.Schema.Value, A]) => Collection(schema)
          )([A] => (schema: Query.Schema.Array.Collection[A]) => schema.self)

      final case class Tuple[A](self: Self.Tuple[Query.Schema.Value, A]) extends Query.Schema.Array[A]

      object Tuple:
        given TupleSchemaInvariant[Query.Schema.Array.Tuple, Query.Schema.Value] =
          TupleSchemaInvariant[Self.Tuple[Query.Schema.Value, *], Query.Schema.Value].imapK(
            [A] => (schema: Self.Tuple[Query.Schema.Value, A]) => Tuple(schema)
          )([A] => (schema: Query.Schema.Array.Tuple[A]) => schema.self)

      given SchemaInvariant[Query.Schema.Array] with
        override def imap[A, B](fa: Query.Schema.Array[A])(f: A => B)(g: B => A): Query.Schema.Array[B] = fa match
          case Collection(self) => Collection(self.imap(f)(g))
          case Tuple(self)      => Tuple(self.imap(f)(g))

        override def enriched[A]: Enriched[Query.Schema.Array[A]] = new Enriched[Query.Schema.Array[A]]:
          override def metadata(a: Query.Schema.Array[A]): Metadata = a match
            case Collection(self) => self.metadata
            case Tuple(self)      => self.metadata

          override def modifyMetadata(a: Query.Schema.Array[A])(f: Metadata => Metadata): Query.Schema.Array[A] =
            a match
              case Collection(self) => Collection(self.metadata(f))
              case Tuple(self)      => Tuple(self.metadata(f))

    final case class Nullable[A](self: Self.Nullable[Query.Schema, A]) extends Query.Schema[A]

    object Nullable:
      given NullableSchemaInvariant[Query.Schema.Nullable, Query.Schema] =
        NullableSchemaInvariant[Self.Nullable[Query.Schema, *], Query.Schema].imapK(
          [A] => (schema: Self.Nullable[Query.Schema, A]) => Nullable(schema)
        )([A] => (schema: Query.Schema.Nullable[A]) => schema.self)

    given SchemaInvariant.Nullable[Query.Schema, Query.Schema.Nullable] with
      override def imap[A, B](fa: Query.Schema[A])(f: A => B)(g: B => A): Query.Schema[B] =
        fa match
          case value: Query.Schema.Value[A]       => value.imap(f)(g)
          case array: Query.Schema.Array[A]       => array.imap(f)(g)
          case nullable: Query.Schema.Nullable[A] => nullable.imap(f)(g)

      override def enriched[A]: Enriched[Query.Schema[A]] = new Enriched[Query.Schema[A]]:
        override def metadata(a: Query.Schema[A]): Metadata = a match
          case value: Query.Schema.Value[A]       => value.metadata
          case array: Query.Schema.Array[A]       => array.metadata
          case nullable: Query.Schema.Nullable[A] => nullable.metadata

        override def modifyMetadata(a: Query.Schema[A])(f: Metadata => Metadata): Query.Schema[A] = a match
          case value: Query.Schema.Value[A]       => value.metadata(f)
          case array: Query.Schema.Array[A]       => array.metadata(f)
          case nullable: Query.Schema.Nullable[A] => nullable.metadata(f)

  enum Style:
    case Form
    case SpaceDelimited
    case PipeDelimited

  type Data = (String, Option[String])

  given SchemaInvariant[Query] with
    override def imap[A, B](fa: Query[A])(f: A => B)(g: B => A): Query[B] =
      fa.copy(value = fa.value.imap(f)(g))

    override def enriched[A]: Enriched[Query[A]] = new Enriched[Query[A]]:
      override def metadata(a: Query[A]): Metadata = a.metadata
      override def modifyMetadata(a: Query[A])(f: Metadata => Metadata): Query[A] =
        a.copy(metadata = f(a.metadata))
