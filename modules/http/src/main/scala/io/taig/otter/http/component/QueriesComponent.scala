package io.taig.otter.http.component

import io.taig.otter.http.operation.QueriesOperation

trait QueriesComponent[F[_], G[_]](F: QueriesOperation[F, G]):
  val QNil: F[Unit] = F.empty
