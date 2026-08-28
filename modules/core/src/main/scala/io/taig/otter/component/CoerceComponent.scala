package io.taig.otter.component

import io.taig.otter.Reference
import io.taig.otter.operation.CoerceOperation

trait CoerceComponent[F[-_, +_], G[-_, +_]](using F: CoerceOperation[F, G]):
  def apply[W, R](schema: => G[W, R]): F[W, R] = F.lift(Reference.later(schema))
