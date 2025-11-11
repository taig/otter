package io.taig.otter.syntax

import io.taig.otter.Collection
import io.taig.otter.Reference
import io.taig.otter.Coerce

trait CollectionSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: F[G, A])(using F: Collection[F, G])
    def schema: Reference[G, ?] = F.schema(self)

object CollectionSyntax extends CollectionSyntax
