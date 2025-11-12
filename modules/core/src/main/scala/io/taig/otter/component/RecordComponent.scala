package io.taig.otter.component

import io.taig.otter.Record

trait RecordComponent[F[+_[a] <: G[a], _], G[_]](using F: Record[F, G]):
  val RNil: F[G, Unit] = F.empty
