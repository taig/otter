package io.taig.otter.syntax

import io.taig.otter.Reference
import io.taig.otter.Tupleable

trait TupleableSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: G[A])(using F: Tupleable[F, G])
    def toTuple: F[G, A] = F.tuple.tuple(schema = Reference.now(self))

object TupleableSyntax extends TupleableSyntax
