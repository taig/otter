package io.taig.otter.component

import io.taig.otter.Field
import io.taig.otter.Record
import io.taig.otter.Reference

trait RecordComponent[F[+_[a] <: G[a], _], G[_]](using operation: Record[F, G]):
  // def field[H[a] <: G[a], A](value: Field[H, A]): F[H, A] = operation.record(field = value)

  // def field[H[a] <: G[a], A](name: String, schema: => H[A]): F[H, A] =
  //   field(Field(name, schema = Reference.later(schema)))

  val RNil: F[G, Unit] = operation.empty
