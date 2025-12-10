package io.taig.otter.component

import io.taig.otter.Field
import io.taig.otter.Reference

trait FieldComponent[F[+_[_], _], G[+_[a] <: H[a], _], H[_]](using F: Field[F, G, H]):
  object field:
    def apply[I[a] <: H[a], A](name: String, schema: => I[A]): F[H, A] =
      F.apply(name, schema = Reference.later(schema))
