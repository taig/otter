package io.taig.otter.http

import io.taig.otter.Reference
import io.taig.otter as Self
import Self.Metadata
import Self.Codec

sealed abstract class Segment[A] extends Product, Serializable:
  def name: String

  def metadata: Metadata

  def modifyMetadata(f: Metadata => Metadata): Segment[A]

  final def imap[B](f: A => B)(g: B => A): Segment[B] = Segment.Modify(self = this, f, g)

  final def toPath: Path[A] = Path.Root(segment = this)

object Segment:
  final private[otter] case class Static(name: String, metadata: Metadata) extends Segment[Unit]:
    override def modifyMetadata(f: Metadata => Metadata): Segment[Unit] = copy(metadata = f(metadata))

  final private[otter] case class Parameter[A](name: String, codec: Reference[Http.Segment, A], metadata: Metadata)
      extends Segment[A]:
    override def modifyMetadata(f: Metadata => Metadata): Segment[A] = copy(metadata = f(metadata))

  final private[otter] case class Modify[A, B](self: Segment[A], f: A => B, g: B => A) extends Segment[B]:
    export self.{metadata, name}
    override def modifyMetadata(f: Metadata => Metadata): Segment[B] = copy(self = self.modifyMetadata(f))

  given Codec[Segment] with
    extension [A](self: Segment[A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Segment[A] = self.modifyMetadata(f)
      override def imap[B](f: A => B)(g: B => A): Segment[B] = self.imap(f)(g)
