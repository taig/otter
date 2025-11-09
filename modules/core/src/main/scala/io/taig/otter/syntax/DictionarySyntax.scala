package io.taig.otter.syntax

import io.taig.otter.Reference
import io.taig.otter.Dictionary

trait DictionarySyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: F[G, A])(using operation: Dictionary[F, G])
    def schema: Reference[G, ?] = operation.schema(self)

object DictionarySyntax extends DictionarySyntax
