package io.taig.otter.syntax

import cats.data.NonEmptyChain
import io.taig.otter.Branch
import io.taig.otter.Union

trait UnionSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: F[G, A])(using F: Union[F, G])
    def branches: NonEmptyChain[Branch[G, ?]] = F.branches(self)

    def orElse[B](schema: F[G, B]): F[G, Either[A, B]] = F.orElse(self, schema)

object UnionSyntax extends UnionSyntax
