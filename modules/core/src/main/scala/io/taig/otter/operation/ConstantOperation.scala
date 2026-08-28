package io.taig.otter.operation

import cats.Eq
import cats.Eval
import io.taig.otter.Reference

/** Constructs the constant type `F` over schemas of type `G`. */
trait ConstantOperation[F[- _, + _], G[- _, + _]]:
  def lift[A](schema: Reference[G, A, A], value: Eval[A], eq: Eq[A]): F[Unit, Unit]

  extension [W, R](fa: F[W, R]) def schema: Reference[G, ?, ?]

object ConstantOperation:
  inline def apply[F[- _, + _], G[- _, + _]](using self: ConstantOperation[F, G]): ConstantOperation[F, G] = self
