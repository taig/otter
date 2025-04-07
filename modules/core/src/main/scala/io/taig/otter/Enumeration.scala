package io.taig.otter

import cats.data.NonEmptyList
import io.taig.enumeration.ext.Mapping
import cats.~>

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

  trait Syntax[Self[_], Value[_]] extends Codec.Syntax[Self]:
    def enumeration[A, B](codec: => Value[A], mapping: Mapping[B, A]): Self[B]

    extension [A](self: Self[A]) def values: NonEmptyList[A]

  object Syntax:
    trait Default[Self[_], Value[_]] extends Enumeration.Syntax[Self, Value]:
      def fromEnumeration[A](codec: Enumeration[Value, A]): Self[A]
      def toEnumeration[A](self: Self[A]): Enumeration[Value, A]

      final override def enumeration[A, B](codec: => Value[A], mapping: Mapping[B, A]): Self[B] =
        fromEnumeration(Enumeration.Root(codec = Reference.later(codec), mapping, metadata = Metadata.Empty))

      extension [A](self: Self[A])
        final override def imap[B](f: A => B)(g: B => A): Self[B] = fromEnumeration(toEnumeration(self).imap(f)(g))
        final override def metadata: Metadata = toEnumeration(self).metadata
        final override def modifyMetadata(f: Metadata => Metadata): Self[A] =
          fromEnumeration(toEnumeration(self).modifyMetadata(f))
