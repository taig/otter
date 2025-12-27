package io.taig.otter.shape

import io.taig.otter as Self

trait SchemaShape:
  type Schema[A] = Self.Schema[?, A]

  object Schema:
    type Of[S[a] <: Schema[a], A] = Self.Schema[S, A]

    type Read[A] = Self.Schema.Read[?, A]

    object Read:
      type Of[S[a] <: Schema.Read[a], A] = Self.Schema.Read[S, A]

    type Write[A] = Self.Schema.Write[?, A]

    object Write:
      type Of[S[a] <: Schema.Write[a], A] = Self.Schema.Write[S, A]

    type Primitive[A] = Self.Schema.Primitive[A]

    object Primitive:
      type Read[A] = Self.Schema.Primitive.Read[A]

      type Write[A] = Self.Schema.Primitive.Write[A]

    type Tuple[A] = Self.Schema.Tuple[?, A]

    object Tuple:
      type Of[S[a] <: Schema[a], A] = Self.Schema.Tuple[S, A]

      type Read[A] = Self.Schema.Tuple.Read[?, A]

      object Read:
        type Of[S[a] <: Schema.Read[a], A] = Self.Schema.Tuple.Read[S, A]

      type Write[A] = Self.Schema.Tuple.Write[?, A]

      object Write:
        type Of[S[a] <: Schema.Write[a], A] = Self.Schema.Tuple.Write[S, A]

object SchemaShape extends SchemaShape
