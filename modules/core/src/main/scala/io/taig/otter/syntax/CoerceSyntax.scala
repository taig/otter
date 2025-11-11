package io.taig.otter.syntax

import io.taig.otter.Coerce
import io.taig.otter.Reference

trait CoerceSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: F[G, A])(using F: Coerce[F, G])
    def schema: Reference[G, ?] = F.schema(self)

object CoerceSyntax extends CoerceSyntax
