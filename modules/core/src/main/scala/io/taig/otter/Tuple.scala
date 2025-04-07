package io.taig.otter

import cats.data.Chain
import cats.~>

// TODO support for optional
sealed abstract class Tuple[+S[_], A] extends Codec[S, A]:
  def codecs: Chain[Reference[S, ?]]
  override def modifyMetadata(f: Metadata => Metadata): Tuple[S, A]
  override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Tuple[T, A]
  final override def imap[B](f: A => B)(g: B => A): Tuple[S, B] = Tuple.Modify(self = this, f, g)
  final def zip[S1[a] >: S[a], B](codec: Tuple[S1, B]): Tuple[S1, (A, B)] =
    Tuple.Zip(left = this, right = codec, metadata = Metadata.Empty)

object Tuple:
  final private[otter] case class Empty(metadata: Metadata) extends Tuple[Nothing, Unit]:
    override def codecs: Chain[Nothing] = Chain.empty
    override def mapK[S1[a] >: Nothing, T[_]](fK: S1 ~> T): Tuple[T, Unit] = this
    override def modifyMetadata(f: Metadata => Metadata): Tuple[Nothing, Unit] = copy(metadata = f(metadata))

  final private[otter] case class Modify[S[_], A, B](self: Tuple[S, A], f: A => B, g: B => A) extends Tuple[S, B]:
    export self.{codecs, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Tuple[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Tuple[T, B] = copy(self = self.mapK(fK))

  final private[otter] case class Root[S[_], A](codec: Reference[S, A], metadata: Metadata) extends Tuple[S, A]:
    override def codecs: Chain[Reference[S, A]] = Chain(codec)
    override def modifyMetadata(f: Metadata => Metadata): Tuple[S, A] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Tuple[T, A] = copy(codec = codec.mapK(fK))

  final private[otter] case class Zip[S[_], A, B](
      left: Tuple[S, A],
      right: Tuple[S, B],
      metadata: Metadata
  ) extends Tuple[S, (A, B)]:
    override def codecs: Chain[Reference[S, ?]] = left.codecs ++ right.codecs
    override def modifyMetadata(f: Metadata => Metadata): Tuple[S, (A, B)] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Tuple[T, (A, B)] =
      copy(left = left.mapK(fK), right = right.mapK(fK))

  trait Syntax[Self[_], Value[_]] extends Codec.Syntax[Self], Invariant.Product[Self, Self]:
    def empty: Self[Unit]
    def one[A](codec: => Value[A]): Self[A]

  object Syntax:
    trait Default[Self[_], Value[_]] extends Syntax[Self, Value]:
      def fromTuple[A](tuple: Tuple[Value, A]): Self[A]
      def toTuple[A](self: Self[A]): Tuple[Value, A]

      final override val empty: Self[Unit] = fromTuple(Tuple.Empty(Metadata.Empty))
      final override def one[A](codec: => Value[A]): Self[A] =
        fromTuple(Tuple.Root(codec = Reference.later(codec), Metadata.Empty))

      extension [A](self: Self[A])
        final def zip[B](codec: Self[B]): Self[(A, B)] = fromTuple(toTuple(self).zip(toTuple(codec)))
        final override def imap[B](f: A => B)(g: B => A): Self[B] = fromTuple(toTuple(self).imap(f)(g))
        final def modifyMetadata(f: Metadata => Metadata): Self[A] =
          fromTuple(toTuple(self).modifyMetadata(f))
        final override def metadata: Metadata = toTuple(self).metadata
