package io.taig.otter.component

import io.taig.otter.operation.TupleOperation

trait TupleComponent[F[_], G[_]](using F: TupleOperation[F, G]):
  final val TNil: F[Unit] = F.empty
