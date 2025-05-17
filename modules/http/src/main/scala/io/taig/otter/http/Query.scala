package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.*
import io.taig.otter.schema.Schema

sealed abstract class Query[A]:
  def name: String

  def schema: Reference[Http.Query, ?]

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
    export self.{explode, metadata, name, schema, style}
    override def modifyExplode(f: Boolean => Boolean): Query[B] = copy(self = self.modifyExplode(f))
    override def modifyStyle(f: Style => Style): Query[B] = copy(self = self.modifyStyle(f))
    override def modifyMetadata(f: Metadata => Metadata): Query[B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Optional[A](self: Query[A]) extends Query[Option[A]]:
    export self.{explode, metadata, name, schema, style}
    override def modifyExplode(f: Boolean => Boolean): Query[Option[A]] = copy(self = self.modifyExplode(f))
    override def modifyStyle(f: Style => Style): Query[Option[A]] = copy(self = self.modifyStyle(f))
    override def modifyMetadata(f: Metadata => Metadata): Query[Option[A]] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[A](
      name: String,
      schema: Reference[Http.Query, A],
      explode: Boolean,
      style: Query.Style,
      metadata: Metadata
  ) extends Query[A]:
    override def modifyExplode(f: Boolean => Boolean): Query[A] = copy(explode = f(explode))
    override def modifyStyle(f: Query.Style => Query.Style): Query[A] = copy(style = f(style))
    override def modifyMetadata(f: Metadata => Metadata): Query[A] = copy(metadata = f(metadata))

  enum Style:
    case Form
    case SpaceDelimited
    case PipeDelimited

  type Data = (String, Option[String])

  given Schema[Query] with
    override def imap[A, B](fa: Query[A])(f: A => B)(g: B => A): Query[B] = fa.imap(f)(g)

    extension [A](self: Query[A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Query[A] = self.modifyMetadata(f)
