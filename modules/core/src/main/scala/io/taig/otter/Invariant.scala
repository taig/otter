package io.taig.otter

import cats.Invariant as CatsInvariant

import scala.annotation.targetName
import scala.compiletime.*

trait Invariant[F[_]]:
  self =>

  extension [A](fa: F[A])
    def imap[B](f: A => B)(g: B => A): F[B]

    // Breaks type inference (https://github.com/typelevel/twiddles/issues/19)
    // final def to[B](using convert: Convert[A, B]): F[B] = imap(convert.to)(convert.from)
    final inline def to[B]: F[B] =
      val convert = summonInline[Convert[A, B]]
      fa.imap(convert.to)(convert.from)

  extension (fa: F[Unit])
    final def as[A](a: A): F[A] = fa.imap(_ => a)(_ => ())

    @targetName("asSingleton")
    final def as[A <: Singleton](a: A): F[A] = fa.imap(_ => a)(_ => ())

  extension [A, B](fa: F[(A, B)])
    final def merge(using merge: Merge[A, B]): F[merge.Out] =
      fa.imap(merge.apply)(merge.unapply)

  final def invariant: CatsInvariant[F] = new CatsInvariant[F]:
    override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = self.imap(fa)(f)(g)

object Invariant:
  trait Coproduct[F[_]] extends Invariant[F]:
    extension [A](self: F[A]) def orElse[B](schema: F[B]): F[Either[A, B]]

  object Coproduct:
    trait Lift[F[_], G[_]] extends Coproduct[G]:
      def lift[A](fa: F[A]): G[A]

      extension [A](self: F[A])
        final def :+[B](schema: F[B]): G[Either[A, B]] = lift(self).orElse(lift(schema))
        final def +:[B](schema: F[B]): G[Either[B, A]] = lift(schema).orElse(lift(self))

      extension [A <: Matchable](self: F[A])
        final inline def |[B <: Matchable](schema: F[B]): G[A | B] = (self :+ schema).imap {
          case Left(a)  => a
          case Right(b) => b
        } {
          case a: A => Left(a)
          case b: B => Right(b)
        }

  trait Product[F[_]] extends Invariant[F]:
    // given result: Invariant[Result]

    // def fromElement[A](schema: Element[A]): F[A]

    extension [A](self: F[A])
      def zip[B](schema: F[B]): F[(A, B)]

      final def merge[B](schema: F[B])(using merge: Merge[A, B]): F[merge.Out] =
        self.zip(schema).imap(merge.apply)(merge.unapply)

      // final def :*[B](schema: Element[B])(using merge: Merge[A, B]): Result[merge.Out] =
      //   self.merge(fromElement(schema))

      // final def *:[B](schema: Element[B])(using merge: Merge[B, A]): Result[merge.Out] =
      //   fromElement(schema).merge(self)
