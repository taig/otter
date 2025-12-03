package io.taig.otter.component

import io.taig.otter.Record
import io.taig.otter.Reference

trait RecordComponent[F[+_[a] <: H[a], _], H[_]](using F: Record[F, H]):
  final def field[I[a] <: H[a], A](name: String, schema: => I[A]): F[I, A] =
    F.field(name, schema = Reference.later(schema))

  final val RNil: F[H, Unit] = F.empty
