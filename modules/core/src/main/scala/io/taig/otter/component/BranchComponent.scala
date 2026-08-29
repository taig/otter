package io.taig.otter.component

import io.taig.otter.Reference
import io.taig.otter.operation.BranchOperation

/** `F` is the branch node, applied to the type of the schema the branch holds. */
trait BranchComponent[Bound[-_, +_], F[_[-w, +r] <: Bound[w, r], -_, +_]]:
  def apply[S[-w, +r] <: Bound[w, r], W, R](name: String, schema: => S[W, R])(using
      F: BranchOperation[[w, r] =>> F[S, w, r], S]
  ): F[S, W, R] = F.lift(name, Reference.later(schema))
