package io.taig.otter.component

import cats.Eq
import io.taig.otter.Constant
import io.taig.otter.Reference

trait ConstantComponent[F[+_[a] <: G[a], _], G[_]](using F: Constant[F, G]):
  def constant[H[a] <: G[a], A](schema: => H[A], value: A)(using eq: Eq[A]): F[H, A] =
    F.constant(schema = Reference.later(schema), value, eq)
