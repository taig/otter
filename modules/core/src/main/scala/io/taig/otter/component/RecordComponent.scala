package io.taig.otter.component

import io.taig.otter.operation.RecordOperation

trait RecordComponent[F[_], G[_]](using F: RecordOperation[F, G]):
  val RNil: F[Unit] = F.empty
