package io.taig.otter.http

import cats.Show
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.Metadata
import io.taig.otter.Reference
import Self.schema.Schema

sealed abstract class Segment[A] extends Product, Serializable:
  def name: String

  def metadata: Metadata

  def modifyMetadata(f: Metadata => Metadata): Segment[A]

  final def imap[B](f: A => B)(g: B => A): Segment[B] = Segment.Modify(self = this, f, g)

  final def toPath: Path[A] = Path.Root(segment = this)

  override def toString: String = this match
    case Segment.Static(name, _)       => name
    case Segment.Parameter(name, _, _) => s"{$name}"
    case Segment.Modify(self, _, _)    => self.toString

object Segment:
  final private[otter] case class Static(name: String, metadata: Metadata) extends Segment[Unit]:
    override def modifyMetadata(f: Metadata => Metadata): Segment[Unit] = copy(metadata = f(metadata))

  final private[otter] case class Parameter[A](name: String, codec: Reference[Http.Parameter, A], metadata: Metadata)
      extends Segment[A]:
    override def modifyMetadata(f: Metadata => Metadata): Segment[A] = copy(metadata = f(metadata))

  final private[otter] case class Modify[A, B](self: Segment[A], f: A => B, g: B => A) extends Segment[B]:
    export self.{metadata, name}
    override def modifyMetadata(f: Metadata => Metadata): Segment[B] = copy(self = self.modifyMetadata(f))

  given Schema[Segment] with
    override def imap[A, B](fa: Segment[A])(f: A => B)(g: B => A): Segment[B] = fa.imap(f)(g)

    extension [A](self: Segment[A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Segment[A] = self.modifyMetadata(f)

  given Show[Segment[?]] = Show.fromToString
