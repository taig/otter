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
