package io.taig.otter.component

import io.taig.otter.Record

trait RecordComponent[F[+_[a] <: H[a], _], H[_]](using F: Record[F, ?, H]):
  val RNil: F[H, Unit] = F.empty
