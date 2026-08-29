package io.taig.otter.component

import io.taig.otter.operation.TupleOperation

trait TupleComponent[Bound[-_, +_], F[_[-w, +r] <: Bound[w, r], -_, +_]]:
  /** The empty tuple. It holds nothing, so its `S` is the bottom constructor and widens to any other. */
  def TNil(using F: TupleOperation[[w, r] =>> F[Nothing, w, r], Nothing]): F[Nothing, Unit, Unit] = F.empty
