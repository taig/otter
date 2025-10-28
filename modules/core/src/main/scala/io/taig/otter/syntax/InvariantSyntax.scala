package io.taig.otter.syntax

import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.Convert
import io.taig.otter.Merge

trait InvariantSyntax:
  extension [F[_]: Invariant, A](self: F[A])
    final def to[B](using convert: Convert[A, B]): F[B] =
      self.imap(convert.to)(convert.from)

  extension [F[_]: Invariant](self: F[Unit]) final def as[A](a: A): F[a.type] = self.imap(_ => a)(_ => ())

  extension [F[_], A, B](self: F[(A, B)])(using Invariant[F])
    final def merge(using merge: Merge[A, B]): F[merge.Out] = self.imap(merge.apply)(merge.unapply)

object InvariantSyntax extends InvariantSyntax
