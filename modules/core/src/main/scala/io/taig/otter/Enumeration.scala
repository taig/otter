package io.taig.otter

import cats.data.NonEmptyList
import io.taig.enumeration.ext.Mapping
import cats.~>
import cats.Invariant

sealed abstract class Enumeration[+S[_], A] extends Codec[S, A]:
  def codec: Reference[S, ?]
  def values: NonEmptyList[A]
  override def modifyMetadata(f: Metadata => Metadata): Enumeration[S, A]
  override def imap[B](f: A => B)(g: B => A): Enumeration[S, B] = Enumeration.Modify(self = this, f, g)
  override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Enumeration[T, A] = ???

object Enumeration:
  final private[otter] case class Modify[S[_], A, B](self: Enumeration[S, A], f: A => B, g: B => A)
      extends Enumeration[S, B]:
    export self.{codec, metadata}
    override def values: NonEmptyList[B] = self.values.map(f)
    override def modifyMetadata(f: Metadata => Metadata): Enumeration[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Enumeration[T, B] = copy(self = self.mapK(fK))

  final private[otter] case class Root[S[_], A, B](
      codec: Reference[S, A],
      mapping: Mapping[B, A],
      metadata: Metadata
  ) extends Enumeration[S, B]:
    override def modifyMetadata(f: Metadata => Metadata): Enumeration[S, B] = copy(metadata = f(metadata))
    override def values: NonEmptyList[B] = mapping.values
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Enumeration[T, B] = copy(codec = codec.mapK(fK))

  given [S[_]]: Invariant[Enumeration[S, *]] with
    override def imap[A, B](fa: Enumeration[S, A])(f: A => B)(g: B => A): Enumeration[S, B] =
      fa.imap(f)(g)
