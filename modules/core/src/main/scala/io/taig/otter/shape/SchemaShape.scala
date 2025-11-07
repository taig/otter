package io.taig.otter.shape

import io.taig.otter as Self

trait SchemaShape:
  type Schema[A] = Self.Schema[?, A]

  object Schema:
    type Of[+S[a] <: Schema[a], A] = Self.Schema[S, A]

    type Collection[A] = Self.Schema.Collection[?, A]

    object Collection:
      type Of[+S[a] <: Schema[a], A] = Self.Schema.Collection[S, A]

object SchemaShape extends SchemaShape
