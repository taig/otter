package io.taig.otter.component

import io.taig.otter.Record
import io.taig.otter.Reference
import io.taig.otter.Field

trait RecordComponent[F[+_[a] <: G[a], _], G[_]]: // (using F: Record[F, G]):
  object field
  // def apply[H[a] <: G[a], A](value: Field[H, A]): F[H, A] = F.apply(value)

  // def apply[H[a] <: G[a], A](name: String, schema: => H[A]): F[H, A] =
  //   apply(Field(name, schema = Reference.later(schema)))

  // final val RNil: F[G, Unit] = F.empty
