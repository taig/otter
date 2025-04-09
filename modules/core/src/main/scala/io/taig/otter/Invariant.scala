package io.taig.otter

import cats.Invariant as CatsInvariant

trait Invariant[Self[_]]:
  self =>

  extension [A](self: Self[A])
    def imap[B](f: A => B)(g: B => A): Self[B]
    final def to[B](using convert: Convert[A, B]): Self[B] = imap(convert.to)(convert.from)

  final def invariant: CatsInvariant[Self] = new CatsInvariant[Self]:
    override def imap[A, B](fa: Self[A])(f: A => B)(g: B => A): Self[B] = self.imap(fa)(f)(g)

object Invariant:
  trait Coproduct[Self[_], Result[_]] extends Invariant[Self]:
    given result: Invariant[Result]

    extension [A](self: Self[A])
      def orElse[B](codec: Self[B]): Result[Either[A, B]]
      final def :+[B](codec: Self[B]): Result[Either[A, B]] = orElse(codec)
      final def +:[B](codec: Self[B]): Result[Either[B, A]] = codec.orElse(self)

    extension [A <: Matchable](self: Self[A])
      final inline def |[B <: Matchable](codec: Self[B]): Result[A | B] = self
        .orElse(codec)
        .imap {
          case Left(a)  => a
          case Right(b) => b
        } {
          case a: A => Left(a)
          case b: B => Right(b)
        }

  trait Product[Self[_], Element[_], Result[_]] extends Invariant[Self]:
    given result: Invariant[Result]

    def fromElement[A](codec: Element[A]): Self[A]

    extension [A](self: Self[A])
      def zip[B](codec: Self[B]): Result[(A, B)]

      def merge[B](codec: Self[B])(using merge: Merge[A, B]): Result[merge.Out] =
        zip(codec).imap(merge.apply)(merge.unapply)

      final def :*[B](codec: Element[B])(using merge: Merge[A, B]): Result[merge.Out] =
        self.merge(fromElement(codec))

      final def *:[B](codec: Element[B])(using merge: Merge[B, A]): Result[merge.Out] =
        fromElement(codec).merge(self)
