package io.taig.otter.component

import cats.Eq
import cats.Eval
import io.taig.otter.Reference
import io.taig.otter.operation.ConstantOperation

trait ConstantComponent[F[-_, +_], G[-_, +_]](using F: ConstantOperation[F, G]):
  def apply[A: Eq](schema: => G[A, A], value: => A): F[Unit, Unit] =
    F.lift(Reference.later(schema), Eval.later(value), Eq[A])
