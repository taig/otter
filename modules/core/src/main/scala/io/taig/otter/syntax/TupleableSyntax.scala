package io.taig.otter.syntax

import io.taig.otter.Reference
import io.taig.otter.Tupleable
import io.taig.otter.Merge
import cats.Invariant
import cats.syntax.all.*

trait TupleableSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: G[A])(using F: Tupleable[F, G])
    def toTuple: F[G, A] = F.tuple.tuple(schema = Reference.now(self))

  // extension [F[+_[a] <: G[a], _], G[_], A](self: G[A])(using F: Tupleable[F, G])(using Invariant[F[G, *]])
  //   def :*[B](schema: G[B])(using merge: Merge[A, B]): F[G, merge.Out] =
  //     F.tuple.zip(self.toTuple, schema.toTuple).imap(merge.apply)(merge.unapply)

object TupleableSyntax extends TupleableSyntax
