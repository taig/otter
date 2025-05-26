package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.*
import io.taig.otter.operation.*
import io.taig.otter as Self

sealed abstract class Query[A]:
  def name: String

  def schema: Reference[Query.Value, ?]

  def isOptional: Boolean

  def explode: Boolean
  def modifyExplode(f: Boolean => Boolean): Query[A]
  final def explode(value: Boolean): Query[A] = modifyExplode(_ => value)

  def style: Query.Style
  def modifyStyle(f: Query.Style => Query.Style): Query[A]
  final def style(value: Query.Style): Query[A] = modifyStyle(_ => value)

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Query[A]

  final def imap[B](f: A => B)(g: B => A): Query[B] = Query.Modify(self = this, f, g)

  final def zip[B](query: Query[B]): Queries[(A, B)] = toQueries.zip(query.toQueries)

  final def toQueries: Queries[A] = Queries.Root(query = this)

object Query:
  final private[otter] case class Modify[A, B](self: Query[A], f: A => B, g: B => A) extends Query[B]:
    export self.{explode, isOptional, metadata, name, schema, style}
    override def modifyExplode(f: Boolean => Boolean): Query[B] = copy(self = self.modifyExplode(f))
    override def modifyStyle(f: Style => Style): Query[B] = copy(self = self.modifyStyle(f))
    override def modifyMetadata(f: Metadata => Metadata): Query[B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Optional[A](self: Query[A]) extends Query[Option[A]]:
    export self.{explode, metadata, name, schema, style}
    override def isOptional: Boolean = true
    override def modifyExplode(f: Boolean => Boolean): Query[Option[A]] = copy(self = self.modifyExplode(f))
    override def modifyStyle(f: Style => Style): Query[Option[A]] = copy(self = self.modifyStyle(f))
    override def modifyMetadata(f: Metadata => Metadata): Query[Option[A]] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[A](
      name: String,
      schema: Reference[Query.Value, A],
      explode: Boolean,
      style: Query.Style,
      metadata: Metadata
  ) extends Query[A]:
    override def isOptional: Boolean = false
    override def modifyExplode(f: Boolean => Boolean): Query[A] = copy(explode = f(explode))
    override def modifyStyle(f: Query.Style => Query.Style): Query[A] = copy(style = f(style))
    override def modifyMetadata(f: Metadata => Metadata): Query[A] = copy(metadata = f(metadata))

  sealed trait Value[A] extends Product with Serializable

  object Value:
    sealed trait Atom[A] extends Query.Value[A]

    object Atom:
      final case class Constant[A](self: Enrichment[Self.Constant[Query.Value.Atom.Primitive, *], A]) extends Atom[A]

      object Constant:
        given EnrichedConstantSchemaInvariant[Query.Value.Atom.Constant, Query.Value.Atom.Primitive] =
          EnrichedConstantSchemaInvariant[Enrichment[
            Self.Constant[Query.Value.Atom.Primitive, *],
            *
          ], Query.Value.Atom.Primitive]
            .imapK(
              [A] => (schema: Enrichment[Self.Constant[Query.Value.Atom.Primitive, *], A]) => Constant(schema)
            )([A] => (value: Query.Value.Atom.Constant[A]) => value.self)

      final case class Enumeration[A](self: Enrichment[Self.Enumeration[Query.Value.Atom.Primitive, *], A])
          extends Atom[A]

      object Enumeration:
        given EnrichedEnumerationSchemaInvariant[Query.Value.Atom.Enumeration, Query.Value.Atom.Primitive] =
          EnrichedEnumerationSchemaInvariant[Enrichment[
            Self.Enumeration[Query.Value.Atom.Primitive, *],
            *
          ], Query.Value.Atom.Primitive]
            .imapK(
              [A] => (schema: Enrichment[Self.Enumeration[Query.Value.Atom.Primitive, *], A]) => Enumeration(schema)
            )([A] => (value: Query.Value.Atom.Enumeration[A]) => value.self)

      final case class Primitive[A](self: Enrichment[Self.Primitive.String, A]) extends Atom[A]

      object Primitive:
        given EnrichedPrimitiveSchemaInvariant.String[Query.Value.Atom.Primitive] =
          EnrichedPrimitiveSchemaInvariant
            .String[Enrichment[Self.Primitive.String, *]]
            .imapK(
              [A] => (schema: Enrichment[Self.Primitive.String, A]) => Primitive(schema)
            )([A] => (value: Query.Value.Atom.Primitive[A]) => value.self)

      final case class Union[A](self: Enrichment[Self.Union[Query.Value.Atom, *], A]) extends Atom[A]

      object Union:
        given EnrichedUnionSchemaInvariant[Query.Value.Atom.Union, Query.Value.Atom] =
          EnrichedUnionSchemaInvariant[Enrichment[Self.Union[Query.Value.Atom, *], *], Query.Value.Atom]
            .imapK(
              [A] => (schema: Enrichment[Self.Union[Query.Value.Atom, *], A]) => Union(schema)
            )([A] => (value: Query.Value.Atom.Union[A]) => value.self)

      given EnrichedSchemaInvariant[Query.Value.Atom] with
        override def imap[A, B](fa: Query.Value.Atom[A])(f: A => B)(g: B => A): Query.Value.Atom[B] = fa match
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

    sealed trait Array[A] extends Query.Value[A]

    object Array:
      final case class Collection[A](self: Enrichment[Self.Collection[Query.Value.Atom, *], A])
          extends Query.Value.Array[A]

      object Collection:
        given EnrichedCollectionSchemaInvariant[Query.Value.Array.Collection, Query.Value.Atom] =
          EnrichedCollectionSchemaInvariant[Enrichment[Self.Collection[Query.Value.Atom, *], *], Query.Value.Atom]
            .imapK(
              [A] => (schema: Enrichment[Self.Collection[Query.Value.Atom, *], A]) => Collection(schema)
            )([A] => (value: Query.Value.Array.Collection[A]) => value.self)

      final case class Tuple[A](self: Enrichment[Self.Tuple[Query.Value.Atom, *], A]) extends Query.Value.Array[A]

      object Tuple:
        given EnrichedTupleSchemaInvariant[Query.Value.Array.Tuple, Query.Value.Atom] = ???

      given EnrichedSchemaInvariant[Query.Value.Array] with
        override def imap[A, B](fa: Query.Value.Array[A])(f: A => B)(g: B => A): Query.Value.Array[B] = fa match
          case Collection(self) => Collection(self.mapF(_.imap(f)(g)))
          case Tuple(self)      => Tuple(self.mapF(_.imap(f)(g)))

        extension [A](self: Array[A])
          override def metadata: Metadata = self match
            case Collection(self) => self.metadata
            case Tuple(self)      => self.metadata

          override def metadata(f: Metadata => Metadata): Array[A] = self match
            case Collection(self) => Collection(self.copy(metadata = f(self.metadata)))
            case Tuple(self)      => Tuple(self.copy(metadata = f(self.metadata)))

    final case class Nullable[A](self: Enrichment[Self.Nullable[Query.Value, *], A]) extends Query.Value[A]

    object Nullable:
      given EnrichedNullableSchemaInvariant[Query.Value.Nullable, Query.Value] = ???

  enum Style:
    case Form
    case SpaceDelimited
    case PipeDelimited

  type Data = (String, Option[String])

  given SchemaInvariant[Query] with
    override def imap[A, B](fa: Query[A])(f: A => B)(g: B => A): Query[B] = fa.imap(f)(g)
