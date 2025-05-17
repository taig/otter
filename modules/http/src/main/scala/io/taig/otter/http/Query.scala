package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.*
import cats.Invariant

sealed abstract class Query[A]:
  def name: String

  def codec: Reference[Http.Query, ?]

  def metadata: Metadata

  def modifyMetadata(f: Metadata => Metadata): Query[A]

  final def imap[B](f: A => B)(g: B => A): Query[B] = Query.Modify(self = this, f, g)

  final def zip[B](query: Query[B]): Queries[(A, B)] = toQueries.zip(query.toQueries)

  final def toQueries: Queries[A] = Queries.Root(query = this)

object Query:
  final private[otter] case class Modify[A, B](self: Query[A], f: A => B, g: B => A) extends Query[B]:
    export self.{codec, metadata, name}
    override def modifyMetadata(f: Metadata => Metadata): Query[B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Optional[A](self: Query[A]) extends Query[Option[A]]:
    export self.{codec, metadata, name}
    override def modifyMetadata(f: Metadata => Metadata): Query[Option[A]] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[A](name: String, codec: Reference[Http.Query, A], metadata: Metadata)
      extends Query[A]:
    override def modifyMetadata(f: Metadata => Metadata): Query[A] = copy(metadata = f(metadata))

  enum Style:
    case Form
    case SpaceDelimited
    case PipeDelimited

  type Data = (String, Option[String])

  given Invariant[Query] with
    override def imap[A, B](fa: Query[A])(f: A => B)(g: B => A): Query[B] = fa.imap(f)(g)
