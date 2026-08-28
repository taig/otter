package io.taig.otter.component

import io.taig.otter.Reference
import io.taig.otter.operation.BranchOperation

trait BranchComponent[F[- _, + _], G[- _, + _]](using F: BranchOperation[F, G]):
  def apply[W, R](name: String, schema: => G[W, R]): F[W, R] = F.lift(name, Reference.later(schema))
