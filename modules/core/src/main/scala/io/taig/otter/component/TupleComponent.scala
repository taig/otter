package io.taig.otter.component

import io.taig.otter.Reference
import io.taig.otter.Tuple

trait TupleComponent[F[+_[a] <: G[a], _], G[_]](using operation: Tuple[F, G]):
  def tuple[H[a] <: G[a], A](schema: Reference[H, A]): F[H, A] = operation.tuple(schema = schema)

  def tuple[H[a] <: G[a], A](schema: => H[A]): F[H, A] = tuple(Reference.later(schema))

  val TNil: F[G, Unit] = operation.empty
