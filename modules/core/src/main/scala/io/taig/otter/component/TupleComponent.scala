package io.taig.otter.component

import io.taig.otter.operation.TupleOperation

trait TupleComponent[Bound[-_, +_], F[_[-w, +r] <: Bound[w, r], -_, +_]]:
  /** The empty tuple. It holds nothing, so its `S` is the bottom constructor and widens to any other.
    *
    * A place to start a chain rather than something a chain needs: two schemas beside each other already are the tuple
    * that holds them, so `TNil :* string :* int`, `string :* int` and `string *: int *: TNil` are one schema.
    */
  def TNil(using F: TupleOperation[[w, r] =>> F[Nothing, w, r], Nothing]): F[Nothing, Unit, Unit] = F.empty
