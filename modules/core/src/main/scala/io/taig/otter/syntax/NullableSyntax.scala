package io.taig.otter.syntax

import cats.Eval
import io.taig.otter.Nullable
import io.taig.otter.Reference

trait NullableSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: G[A])(using F: Nullable[F, G])
    def nullable: F[G, Option[A]] = F.nullish.nullable(schema = Reference.now(self))

    def nullable(default: => A): F[G, A] =
      F.nullish.nullable(schema = Reference.now(self), default = Eval.later(default))

object NullableSyntax extends NullableSyntax
