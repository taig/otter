package io.taig.otter.syntax

import io.taig.otter.Reference
import io.taig.otter.Tupleable
import io.taig.otter.Merge
import cats.Invariant
import cats.syntax.all.*
import io.taig.otter.Recordeable

trait RecordeableSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: G[A])(using F: Recordeable[F, G])
    def toRecord: F[G, A] = F.record.record(Reference.now(self))

object RecordeableSyntax extends RecordeableSyntax
