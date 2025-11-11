package io.taig.otter.syntax

import io.taig.otter.Constant
import io.taig.otter.Reference

trait ConstantSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: F[G, A])(using F: Constant[F, G])
    def schema: Reference[G, ?] = F.schema(self)

object ConstantSyntax extends ConstantSyntax
