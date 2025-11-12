package io.taig.otter.syntax

import cats.data.Chain
import io.taig.otter.Reference
import io.taig.otter.Tuple
import cats.syntax.all.*
import cats.Invariant
import io.taig.otter.Merge

trait TupleSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: F[G, A])(using F: Tuple[F, G])
    def schemas: Chain[Reference[G, ?]] = F.schemas(self)

    def zip[B](schema: F[G, B]): F[G, (A, B)] = F.zip(self, schema)

  extension [F[+_[a] <: G[a], _], G[_], A](self: F[G, A])(using Tuple[F, G], Invariant[F[G, *]])
    def :*[B](schema: F[G, B])(using merge: Merge[A, B]): F[G, merge.Out] =
      self.zip(schema).imap(merge.apply)(merge.unapply)

object TupleSyntax extends TupleSyntax
