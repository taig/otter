package io.taig.otter.component

import io.taig.otter.Field
import io.taig.otter.Record
import io.taig.otter.Reference

trait FieldComponent[F[+_[a] <: G[a], _], G[_]](using F: Field[F, G]):
  def field[H[a] <: G[a], A](name: String, schema: => H[A]): F[H, A] =
    F.field(name, schema = Reference.later(schema))
