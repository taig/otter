package io.taig.otter.syntax

import io.taig.otter.Reference
import io.taig.otter.Coerce
import scala.compiletime.summonFrom

trait CoerceSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: F[G, A])(using operation: Coerce[F, G])
    def schema: Reference[G, ?] = operation.schema(self)

  extension [F[+_[a] <: G[a], _], G[_], A](self: G[A])
    def coerce(using operation: Coerce[F, G]): F[G, A] = operation.coerce(schema = Reference.now(self))

object CoerceSyntax extends CoerceSyntax
