package io.taig.otter.syntax

import io.taig.otter.Enumeration
import io.taig.otter.Reference

trait EnumerationSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: F[G, A])(using F: Enumeration[F, G])
    def schema: Reference[G, ?] = F.schema(self)

object EnumerationSyntax extends EnumerationSyntax
