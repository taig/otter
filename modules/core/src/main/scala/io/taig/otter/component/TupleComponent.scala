package io.taig.otter.component

import io.taig.otter.operation.TupleOperation

trait TupleComponent[F[- _, + _], G[- _, + _]](using F: TupleOperation[F, G]):
  /** The empty tuple. */
  val TNil: F[Unit, Unit] = F.empty
