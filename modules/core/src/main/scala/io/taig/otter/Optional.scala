package io.taig.otter

import cats.~>

sealed abstract class Optional[+S[_], A] extends Codec[S, A]:
  def codec: Reference[S, ?]
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Optional[S, A]
  override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Optional[T, A]
  final override def imap[B](f: A => B)(g: B => A): Optional[S, B] = Optional.Modify(self = this, f, g)

object Optional:
  final private[otter] case class Modify[S[_], A, B](self: Optional[S, A], f: A => B, g: B => A) extends Optional[S, B]:
    export self.{codec, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Optional[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Optional[T, B] = copy(self = self.mapK(fK))

  final private[otter] case class Default[S[_], A](codec: Reference[S, A], default: A, metadata: Metadata)
      extends Optional[S, A]:
    override def modifyMetadata(f: Metadata => Metadata): Optional[S, A] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Optional[T, A] = copy(codec = codec.mapK(fK))

  final private[otter] case class Nullable[S[_], A](codec: Reference[S, A], metadata: Metadata)
      extends Optional[S, Option[A]]:
    override def modifyMetadata(f: Metadata => Metadata): Optional[S, Option[A]] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Optional[T, Option[A]] = copy(codec = codec.mapK(fK))

  trait Syntax[Self[_], Value[_]] extends Codec.Syntax[Self]:
    def nullable[A](codec: Value[A]): Self[Option[A]]
    def nullable[A](codec: Value[A], default: A): Self[A]

  object Syntax:
    trait Default[Self[_], Value[_]] extends Syntax[Self, Value]:
      def fromOptional[A](optional: Optional[Value, A]): Self[A]
      def toOptional[A](self: Self[A]): Optional[Value, A]

      final override def nullable[A](codec: Value[A]): Self[Option[A]] =
        fromOptional(Optional.Nullable(codec = Reference.later(codec), metadata = Metadata.Empty))

      final override def nullable[A](codec: Value[A], default: A): Self[A] =
        fromOptional(Optional.Default(codec = Reference.later(codec), default, metadata = Metadata.Empty))

      extension [A](self: Self[A])
        final override def imap[B](f: A => B)(g: B => A): Self[B] = fromOptional(toOptional(self).imap(f)(g))
        final override def metadata: Metadata = toOptional(self).metadata
        final override def modifyMetadata(f: Metadata => Metadata): Self[A] =
          fromOptional(toOptional(self).modifyMetadata(f))
