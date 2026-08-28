package io.taig.otter.component

import cats.Eq
import cats.Eval
import io.taig.otter.Reference
import io.taig.otter.operation.ConstantOperation

trait ConstantComponent[F[-_, +_], F1[A] >: F[A, A] <: F[A, A], G[-_, +_], G1[A] >: G[A, A] <: G[A, A]](using
    F: ConstantOperation[F, G]
):
  def apply[A: Eq](schema: => G1[A], value: => A): F1[Unit] =
    F.lift(Reference.later(schema), Eval.later(value), Eq[A])
