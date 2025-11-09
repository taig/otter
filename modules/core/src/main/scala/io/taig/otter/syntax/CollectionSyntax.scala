package io.taig.otter.syntax

import io.taig.otter.Collection
import io.taig.otter.Reference

trait CollectionSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: F[G, A])(using operation: Collection[F, G])
    def schema: Reference[G, ?] = operation.schema(self)

object CollectionSyntax extends CollectionSyntax
