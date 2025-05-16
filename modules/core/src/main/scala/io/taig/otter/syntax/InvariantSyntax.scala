package io.taig.otter.syntax

import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.Convert
import io.taig.otter.Merge

import scala.annotation.targetName
import scala.compiletime.*

trait InvariantSyntax:
  extension [F[_]: Invariant, A](fa: F[A])
    // Breaks type inference (https://github.com/typelevel/twiddles/issues/19)
    // final def to[B](using convert: Convert[A, B]): F[B] = imap(convert.to)(convert.from)
    final inline def to[B]: F[B] =
      val convert = summonInline[Convert[A, B]]
      fa.imap(convert.to)(convert.from)

  extension [F[_]: Invariant](fa: F[Unit])
    final def as[A](a: A): F[A] = fa.imap(_ => a)(_ => ())

    @targetName("asSingleton")
    final def as[A <: Singleton](a: A): F[A] = fa.imap(_ => a)(_ => ())

  extension [F[_]: Invariant, A, B](fa: F[(A, B)])
    final def merge(using merge: Merge[A, B]): F[merge.Out] =
      fa.imap(merge.apply)(merge.unapply)

object InvariantSyntax extends InvariantSyntax
