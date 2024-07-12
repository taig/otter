package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.*
import org.typelevel.ci.CIString
import scala.Product as SProduct

sealed trait Header[A] extends Header.Reader[A], Header.Writer[A]:
  override def schema: Value[String, ?, ?]
  final def imap[B](f: A => B)(g: B => A): Header[B] = Header.Transform(this, f, g)
  def update(f: Metadata => Metadata): Header[A]

object Header:
  sealed trait Reader[+A] extends SProduct, Serializable:
    final def map[B](f: A => B): Header.Reader[B] = Reader.Transform(this, f)
    def metadata: Metadata
    def name: CIString
    def schema: Value.Reader[String, ?, ?]
    def update(f: Metadata => Metadata): Header.Reader[A]

  object Reader:
    final case class Root[A](metadata: Metadata, name: CIString, schema: Value.Reader[String, ?, A])
        extends Header.Reader[A]:
      override def update(f: Metadata => Metadata): Header.Reader[A] = copy(metadata = f(metadata))

    final case class Transform[A, B](self: Header.Reader[A], f: A => B) extends Header.Reader[B]:
      export self.{metadata, name, schema}
      override def update(f: Metadata => Metadata): Header.Reader[B] = copy(self = self.update(f))

  sealed trait Writer[-A] extends SProduct, Serializable:
    final def contramap[B](f: B => A): Header.Writer[B] = Writer.Transform(this, f)
    def metadata: Metadata
    def name: CIString
    def schema: Value.Writer[String, ?, ?]
    def update(f: Metadata => Metadata): Header.Writer[A]

  object Writer:
    final case class Root[A](metadata: Metadata, name: CIString, schema: Value.Writer[String, ?, A])
        extends Header.Writer[A]:
      override def update(f: Metadata => Metadata): Header.Writer[A] = copy(metadata = f(metadata))

    final case class Transform[A, B](self: Header.Writer[A], f: B => A) extends Header.Writer[B]:
      export self.{metadata, name, schema}
      override def update(f: Metadata => Metadata): Header.Writer[B] = copy(self = self.update(f))

  final case class Root[A](metadata: Metadata, name: CIString, schema: Value[String, ?, A]) extends Header[A]:
    override def update(f: Metadata => Metadata): Header[A] = copy(metadata = f(metadata))

  final case class Transform[A, B](self: Header[A], f: A => B, g: B => A) extends Header[B]:
    export self.{metadata, name, schema}
    override def update(f: Metadata => Metadata): Header[B] = copy(self = self.update(f))
