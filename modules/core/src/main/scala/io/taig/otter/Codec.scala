package io.taig.otter
import cats.~>

abstract class Codec[+S[_], A] extends Product with Serializable:
  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Codec[S, A]
  def imap[B](f: A => B)(g: B => A): Codec[S, B]
  def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Codec[T, A]

object Codec:
  trait Syntax[Self[_]] extends Invariant[Self]:
    extension [A](self: Self[A])
      def metadata: Metadata
      def modifyMetadata(f: Metadata => Metadata): Self[A]

  object Syntax:
    trait Nullable[Self[_], Optional[_]] extends Codec.Syntax[Self]:
      given optional: Optional.Syntax[Optional, Self]

      extension [A](self: Self[A])
        final def nullable: Optional[Option[A]] = optional.nullable(codec = self)
        final def nullable(default: A): Optional[A] = optional.nullable(codec = self, default)

    trait Tupleable[Self[_], Tuple[_]] extends Invariant.Product[Self, Tuple]:
      given tuple: Tuple.Syntax[Tuple, Self]

      extension [A](self: Self[A])
        final override def zip[B](codec: Self[B]): Tuple[(A, B)] = self.toTuple.zip(codec.toTuple)
        final def toTuple: Tuple[A] = tuple.one(self)
