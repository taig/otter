package io.taig.otter.syntax

import io.taig.otter.operation.CollectionOperation

trait CollectionSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](fa: F[G, A])(using operation: CollectionOperation[F, G])
    def ++(other: F[G, A]): F[G, A] = fa

object CollectionSyntax extends CollectionSyntax
