package io.taig.otter.http

import io.taig.otter as Self
import io.taig.otter.*
import io.taig.otter.schema.Schema
import org.typelevel.ci.CIString

sealed abstract class Header[A] extends Product, Serializable:
  def name: CIString

  def schema: Reference[Http.Header, ?]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Header[A]

  final def imap[B](f: A => B)(g: B => A): Header[B] = Header.Modify(self = this, f, g)

  final def toHeaders: Headers[A] = Headers.Root(header = this)

object Header:
  final private[otter] case class Root[A](
      name: CIString,
      schema: Reference[Http.Header, A],
      metadata: Metadata
  ) extends Header[A]:
    override def modifyMetadata(f: Metadata => Metadata): Header[A] = copy(metadata = f(metadata))

  final private[otter] case class Optional[A](self: Header[A]) extends Header[Option[A]]:
    export self.{metadata, name, schema}
    override def modifyMetadata(f: Metadata => Metadata): Header[Option[A]] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Modify[A, B](self: Header[A], f: A => B, g: B => A) extends Header[B]:
    export self.{metadata, name, schema}
    override def modifyMetadata(f: Metadata => Metadata): Header[B] = copy(self = self.modifyMetadata(f))

  enum Style:
    case Label
    case Matrix
    case Simple

  type Data = (CIString, String)

  given Schema[Header] with
    override def imap[A, B](fa: Header[A])(f: A => B)(g: B => A): Header[B] = fa.imap(f)(g)

    extension [A](self: Header[A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Header[A] = self.modifyMetadata(f)
