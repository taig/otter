package io.taig.otter.syntax

import cats.Invariant
import cats.derived.*
import cats.syntax.all.*
import io.taig.otter.Merge

trait InvariantSyntax:
  extension [F[_]](self: F[Unit])(using Invariant[F]) final def as[A](a: A): F[a.type] = self.imap(_ => a)(_ => ())

  extension [F[_], A, B](self: F[(A, B)])(using Invariant[F])
    final def merge(using merge: Merge[A, B]): F[merge.Out] = self.imap(merge.apply)(merge.unapply)

object InvariantSyntax extends InvariantSyntax
