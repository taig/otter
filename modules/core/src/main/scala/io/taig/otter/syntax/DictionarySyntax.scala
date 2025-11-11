package io.taig.otter.syntax

import io.taig.otter.Dictionary
import io.taig.otter.Reference

trait DictionarySyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: F[G, A])(using F: Dictionary[F, G])
    def schema: Reference[G, ?] = F.schema(self)

object DictionarySyntax extends DictionarySyntax
