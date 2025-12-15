package io.taig.otter.component

import io.taig.otter.Field
import io.taig.otter.Reference

trait FieldComponent[F[+_[a] <: H[a], _], H[_]](using F: Field[F, H]):
  object field:
    def apply[I[a] <: H[a], A](name: String, schema: => I[A]): F[I, A] =
      F.apply(name, schema = Reference.later(schema))
