package io.taig.otter.component

import cats.Eq
import cats.Eval
import io.taig.otter.Reference
import io.taig.otter.operation.ConstantOperation

trait ConstantComponent[F[_[-_, +_], -_, +_]]:
  def apply[S[-_, +_], A](schema: => S[A, A], value: => A)(using
      A: Eq[A],
      F: ConstantOperation[[w, r] =>> F[S, w, r], S]
  ): F[S, Unit, Unit] = F.lift(Reference.later(schema), Eval.later(value), A)
