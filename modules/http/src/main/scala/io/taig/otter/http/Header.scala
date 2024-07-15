package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.*
import org.typelevel.ci.CIString
import scala.Product as SProduct

sealed trait Header[A] extends SProduct, Serializable:
  final def imap[B](f: A => B)(g: B => A): Header[B] = Header.Transform(this, f, g)
  def metadata: Metadata
  def name: CIString
  def schema: Value[?, ?]
  def update(f: Metadata => Metadata): Header[A]

object Header:
  final case class Root[A](metadata: Metadata, name: CIString, schema: Value[?, A]) extends Header[A]:
    override def update(f: Metadata => Metadata): Header[A] = copy(metadata = f(metadata))

  final case class Transform[A, B](self: Header[A], f: A => B, g: B => A) extends Header[B]:
    export self.{metadata, name, schema}
    override def update(f: Metadata => Metadata): Header[B] = copy(self = self.update(f))
