package io.taig.otter.component

import io.taig.otter.Reference
import io.taig.otter.operation.FieldOperation
import io.taig.otter.operation.RecordOperation

trait RecordComponent[Bound[-_, +_], F[_[-w, +r] <: Bound[w, r], -_, +_], G[_[-w, +r] <: Bound[w, r], -_, +_]]:
  /** The empty record. It holds nothing, so its `S` is the bottom constructor and widens to any other. */
  def RNil(using
      F: RecordOperation[[w, r] =>> F[Nothing, w, r], [w, r] =>> G[Nothing, w, r]]
  ): F[Nothing, Unit, Unit] = F.empty

object RecordComponent:
  trait Field[Bound[-_, +_], F[_[-w, +r] <: Bound[w, r], -_, +_]]:
    def apply[S[-w, +r] <: Bound[w, r], W, R](name: String, schema: => S[W, R])(using
        F: FieldOperation[[w, r] =>> F[S, w, r], S]
    ): F[S, W, R] = F.lift(name, Reference.later(schema))
