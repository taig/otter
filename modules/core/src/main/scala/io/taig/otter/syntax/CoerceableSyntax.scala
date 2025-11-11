package io.taig.otter.syntax

import io.taig.otter.Coerceable
import io.taig.otter.Reference

trait CoerceableSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: G[A])(using F: Coerceable[F, G])
    def coerce: F[G, A] = F.coerce.coerce(schema = Reference.now(self))

object CoerceableSyntax extends CoerceableSyntax
