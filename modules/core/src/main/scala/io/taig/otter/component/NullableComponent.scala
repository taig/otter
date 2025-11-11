package io.taig.otter.component

import cats.Eval
import io.taig.otter.Nullable
import io.taig.otter.Reference

trait NullableComponent[F[+_[a] <: G[a], _], G[_]](using operation: Nullable[F, G]):
  def nullable[H[a] <: G[a], A](schema: => H[A]): F[H, Option[A]] = operation.nullable(schema = Reference.later(schema))

  def nullable[H[a] <: G[a], A](schema: => H[A], default: => A): F[H, A] =
    operation.nullable(schema = Reference.later(schema), default = Eval.later(default))
