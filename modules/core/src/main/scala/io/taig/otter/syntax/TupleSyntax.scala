package io.taig.otter.syntax

import cats.data.Chain
import io.taig.otter.Reference
import io.taig.otter.Tuple

trait TupleSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: F[G, A])(using F: Tuple[F, G])
    def schemas: Chain[Reference[G, ?]] = F.schemas(self)

    def zip[B](schema: => F[G, B]): F[G, (A, B)] = F.zip(self, schema)

object TupleSyntax extends TupleSyntax
