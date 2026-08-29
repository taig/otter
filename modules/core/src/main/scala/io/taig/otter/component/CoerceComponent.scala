package io.taig.otter.component

import io.taig.otter.Reference
import io.taig.otter.operation.CoerceOperation

trait CoerceComponent[Bound[-_, +_], F[_[-w, +r] <: Bound[w, r], -_, +_]]:
  def apply[S[-w, +r] <: Bound[w, r], W, R](schema: => S[W, R])(using
      F: CoerceOperation[[w, r] =>> F[S, w, r], S]
  ): F[S, W, R] = F.lift(Reference.later(schema))
