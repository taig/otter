package io.taig.otter.syntax

import io.taig.otter.Nullish
import io.taig.otter.Reference

trait NullishSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: F[G, A])(using F: Nullish[F, G])
    def schema: Reference[G, ?] = F.schema(self)

object NullishSyntax extends NullishSyntax
