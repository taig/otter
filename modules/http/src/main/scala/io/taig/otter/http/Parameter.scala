package io.taig.otter.http

import cats.Show
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.Metadata
import io.taig.otter.Reference
import io.taig.otter.schema.Schema

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
      schema: Reference[Http.Parameter, A],
      style: Parameter.Style,
      metadata: Metadata
  ) extends Parameter[A]:
    override def modifyStyle(f: Style => Style): Parameter[A] = copy(style = f(style))
    override def modifyMetadata(f: Metadata => Metadata): Parameter[A] = copy(metadata = f(metadata))

  final private[otter] case class Modify[A, B](self: Parameter[A], f: A => B, g: B => A) extends Parameter[B]:
    export self.{metadata, name, style}
    override def modifyStyle(f: Style => Style): Parameter[B] = copy(self = self.modifyStyle(f))
    override def modifyMetadata(f: Metadata => Metadata): Parameter[B] = copy(self = self.modifyMetadata(f))

  enum Style:
    case Simple, Label, Matrix

  given Schema[Parameter] with
    override def imap[A, B](fa: Parameter[A])(f: A => B)(g: B => A): Parameter[B] = fa.imap(f)(g)

    extension [A](self: Parameter[A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Parameter[A] = self.modifyMetadata(f)

  given [A]: Show[Parameter[A]] = Show.fromToString
