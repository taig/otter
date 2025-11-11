package io.taig.otter.syntax

import cats.Eval
import io.taig.otter.Nullable
import io.taig.otter.Reference

import scala.compiletime.summonFrom

trait NullableSyntax:
  extension [F[+_[a] <: G[a], _], G[_], A](self: F[G, A])(using F: Nullable[F, G])
    def schema: Reference[G, ?] = F.schema(self)

  extension [F[+_[a] <: G[a], _], G[_], A](self: G[A])(using F: Nullable[F, G])
    def nullable: F[G, Option[A]] = F.nullable(Reference.now(self))

    def nullable(default: => A): F[G, A] = F.nullable(Reference.now(self), default = Eval.later(default))

object NullableSyntax extends NullableSyntax
