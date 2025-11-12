package io.taig.otter.syntax

import cats.Invariant
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Field
import io.taig.otter.Merge
import io.taig.otter.Record
import io.taig.otter.Reference

trait FieldSyntax
// extension [F[+_[a] <: G[a], _], G[_], A](self: F[G, A])(using Record[F, G], Invariant[F[G, *]])
//   def toRecord[B](schema: F[G, B])(using merge: Merge[A, B]): F[G, merge.Out] =
//     self.zip(schema).imap(merge.apply)(merge.unapply)

object FieldSyntax extends FieldSyntax
