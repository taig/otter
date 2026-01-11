package io.taig.otter.component

import io.taig.otter.operation.TupleOperation

trait TupleComponent[F[_], G[_]](using F: TupleOperation[F, G]):
  final def TNil: F[Unit] = F.empty
