package io.taig.otter.component

import io.taig.otter.Record

trait RecordComponent[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using F: Record[F, G, H]):
  final val RNil: F[Nothing, Unit] = F.empty
