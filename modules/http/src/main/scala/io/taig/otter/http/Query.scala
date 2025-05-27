package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.*
import io.taig.otter.operation.*
import io.taig.otter as Self

type Query[A] = Enrichment[Query.Value, A]

object Query:
  sealed abstract class Value[A]:
    def name: String

    def schema: Reference[Query.Schema, ?]

    def isOptional: Boolean

    def explode: Boolean
    def modifyExplode(f: Boolean => Boolean): Query.Value[A]

    def style: Query.Style
    def modifyStyle(f: Query.Style => Query.Style): Query.Value[A]

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

  sealed trait Schema[A] extends Product with Serializable

  object Schema:
    sealed trait Value[A] extends Query.Schema[A]

    object Value:
      final case class Constant[A](self: Enrichment[Self.Constant[Query.Schema.Value.Primitive, *], A]) extends Value[A]

      object Constant:
        given EnrichedConstantSchemaInvariant[Query.Schema.Value.Constant, Query.Schema.Value.Primitive] =
          EnrichedConstantSchemaInvariant[Enrichment[
            Self.Constant[Query.Schema.Value.Primitive, *],
            *
          ], Query.Schema.Value.Primitive]
            .imapK(
              [A] => (schema: Enrichment[Self.Constant[Query.Schema.Value.Primitive, *], A]) => Constant(schema)
            )([A] => (schema: Query.Schema.Value.Constant[A]) => schema.self)

      final case class Enumeration[A](self: Enrichment[Self.Enumeration[Query.Schema.Value.Primitive, *], A])
          extends Value[A]

      object Enumeration:
        given EnrichedEnumerationSchemaInvariant[Query.Schema.Value.Enumeration, Query.Schema.Value.Primitive] =
          EnrichedEnumerationSchemaInvariant[Enrichment[
            Self.Enumeration[Query.Schema.Value.Primitive, *],
            *
          ], Query.Schema.Value.Primitive]
            .imapK(
              [A] => (schema: Enrichment[Self.Enumeration[Query.Schema.Value.Primitive, *], A]) => Enumeration(schema)
            )([A] => (schema: Query.Schema.Value.Enumeration[A]) => schema.self)

      final case class Primitive[A](self: Enrichment[Self.Primitive.String, A]) extends Value[A]

      object Primitive:
        given EnrichedPrimitiveSchemaInvariant.String[Query.Schema.Value.Primitive] =
          EnrichedPrimitiveSchemaInvariant
            .String[Enrichment[Self.Primitive.String, *]]
            .imapK(
              [A] => (schema: Enrichment[Self.Primitive.String, A]) => Primitive(schema)
            )([A] => (schema: Query.Schema.Value.Primitive[A]) => schema.self)

      final case class Union[A](self: Enrichment[Self.Union[Query.Schema.Value, *], A]) extends Value[A]

      object Union:
        given EnrichedUnionSchemaInvariant[Query.Schema.Value.Union, Query.Schema.Value] =
          EnrichedUnionSchemaInvariant[Enrichment[Self.Union[Query.Schema.Value, *], *], Query.Schema.Value]
            .imapK(
              [A] => (schema: Enrichment[Self.Union[Query.Schema.Value, *], A]) => Union(schema)
            )([A] => (schema: Query.Schema.Value.Union[A]) => schema.self)

      given EnrichedSchemaInvariant[Query.Schema.Value] with
        override def imap[A, B](fa: Query.Schema.Value[A])(f: A => B)(g: B => A): Query.Schema.Value[B] = fa match
          case Constant(self)    => Constant(self.mapF(_.imap(f)(g)))
          case Enumeration(self) => Enumeration(self.mapF(_.imap(f)(g)))
          case Primitive(self)   => Primitive(self.mapF(_.imap(f)(g)))
          case Union(self)       => Union(self.mapF(_.imap(f)(g)))

        extension [A](self: Value[A])
          override def metadata: Metadata = self match
            case Constant(self)    => self.metadata
            case Enumeration(self) => self.metadata
            case Primitive(self)   => self.metadata
            case Union(self)       => self.metadata

          override def metadata(f: Metadata => Metadata): Value[A] = self match
            case Constant(self)    => Constant(self.copy(metadata = f(self.metadata)))
            case Enumeration(self) => Enumeration(self.copy(metadata = f(self.metadata)))
            case Primitive(self)   => Primitive(self.copy(metadata = f(self.metadata)))
            case Union(self)       => Union(self.copy(metadata = f(self.metadata)))

    sealed trait Array[A] extends Query.Schema[A]

    object Array:
      final case class Collection[A](self: Enrichment[Self.Collection[Query.Schema.Value, *], A])
          extends Query.Schema.Array[A]

      object Collection:
        given EnrichedCollectionSchemaInvariant[Query.Schema.Array.Collection, Query.Schema.Value] =
          EnrichedCollectionSchemaInvariant[Enrichment[Self.Collection[Query.Schema.Value, *], *], Query.Schema.Value]
            .imapK(
              [A] => (schema: Enrichment[Self.Collection[Query.Schema.Value, *], A]) => Collection(schema)
            )([A] => (schema: Query.Schema.Array.Collection[A]) => schema.self)

      final case class Tuple[A](self: Enrichment[Self.Tuple[Query.Schema.Value, *], A]) extends Query.Schema.Array[A]

      object Tuple:
        given EnrichedTupleSchemaInvariant[Query.Schema.Array.Tuple, Query.Schema.Value] = ???

      given EnrichedSchemaInvariant[Query.Schema.Array] with
        override def imap[A, B](fa: Query.Schema.Array[A])(f: A => B)(g: B => A): Query.Schema.Array[B] = fa match
          case Collection(self) => Collection(self.mapF(_.imap(f)(g)))
          case Tuple(self)      => Tuple(self.mapF(_.imap(f)(g)))

        extension [A](self: Array[A])
          override def metadata: Metadata = self match
            case Collection(self) => self.metadata
            case Tuple(self)      => self.metadata

          override def metadata(f: Metadata => Metadata): Array[A] = self match
            case Collection(self) => Collection(self.copy(metadata = f(self.metadata)))
            case Tuple(self)      => Tuple(self.copy(metadata = f(self.metadata)))

    final case class Nullable[A](self: Enrichment[Self.Nullable[Query.Schema, *], A]) extends Query.Schema[A]

    object Nullable:
      given EnrichedNullableSchemaInvariant[Query.Schema.Nullable, Query.Schema] = ???

  enum Style:
    case Form
    case SpaceDelimited
    case PipeDelimited

  type Data = (String, Option[String])

  extension [A](self: Query[A]) def name: String = self.self.name

  // given SchemaInvariant[Query] with
  //   override def imap[A, B](fa: Query[A])(f: A => B)(g: B => A): Query[B] = fa.imap(f)(g)
