package io.taig.otter

import cats.Invariant as CatsInvariant

import scala.annotation.targetName
import scala.compiletime.*

trait Invariant[Self[_]]:
  self =>

  extension [A](self: Self[A])
    def imap[B](f: A => B)(g: B => A): Self[B]
    // Breaks type inference (https://github.com/typelevel/twiddles/issues/19)
    // final def to[B](using convert: Convert[A, B]): Self[B] = imap(convert.to)(convert.from)
    final inline def to[B]: Self[B] =
      val convert = summonInline[Convert[A, B]]
      self.imap(convert.to)(convert.from)

  extension (self: Self[Unit])
    @targetName("asSingleton")
    def as[A](a: A): Self[A] = self.imap(_ => a)(_ => ())
    def as[A <: Singleton](a: A): Self[A] = self.imap(_ => a)(_ => ())

  extension [A, B](self: Self[(A, B)])
    final def merge(using merge: Merge[A, B]): Self[merge.Out] =
      self.imap(merge.apply)(merge.unapply)

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
