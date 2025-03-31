package io.taig.otter

import cats.data.NonEmptyList
import io.taig.enumeration.ext.Mapping

sealed abstract class Enumeration[A] extends Codec[Nothing, A]:
  def codec: Primitive[?]
  def values: NonEmptyList[A]
  override def modifyMetadata(f: Metadata => Metadata): Enumeration[A]
  override def imap[B](f: A => B)(g: B => A): Enumeration[B] = Enumeration.Modify(self = this, f, g)

object Enumeration:
  final private[otter] case class Modify[S[_], A, B](self: Enumeration[A], f: A => B, g: B => A) extends Enumeration[B]:
    export self.{codec, metadata}
    override def values: NonEmptyList[B] = self.values.map(f)
    override def modifyMetadata(f: Metadata => Metadata): Enumeration[B] = copy(self = self.modifyMetadata(f))

  final private[otter] case class Root[A, B](
      codec: Primitive[A],
      mapping: Mapping[B, A],
      metadata: Metadata
  ) extends Enumeration[B]:
    override def modifyMetadata(f: Metadata => Metadata): Enumeration[B] = copy(metadata = f(metadata))
    override def values: NonEmptyList[B] = mapping.values

  given CodecInvariant[Enumeration] with
    override def imap[A, B](fa: Enumeration[A])(f: A => B)(g: B => A): Enumeration[B] = fa.imap(f)(g)
