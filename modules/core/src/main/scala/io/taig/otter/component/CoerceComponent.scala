package io.taig.otter.component

import io.taig.otter.Reference
import io.taig.otter.operation.CoerceOperation

trait CoerceComponent[F[_[-_, +_], -_, +_]]:
  def apply[S[-_, +_], W, R](schema: => S[W, R])(using
      F: CoerceOperation[[w, r] =>> F[S, w, r], S]
  ): F[S, W, R] = F.lift(Reference.later(schema))
