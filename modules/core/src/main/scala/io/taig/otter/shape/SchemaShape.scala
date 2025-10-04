package io.taig.otter.shape

import io.taig.otter as Self

trait SchemaShape:
  type Schema[A] = Self.Schema[?, A]

  object Schema:
    type Of[S[a] <: Self.Schema[?, a], A] = Self.Schema[S, A]

    type Collection[A] = Self.Schema.Collection[?, A]

    object Collection:
      type Of[S[a] <: Self.Schema[?, a], A] = Self.Schema.Collection[S, A]

    type Primitive[A] = Self.Schema.Primitive[A]

    object Primitive:
      type Boolean[A] = Self.Schema.Primitive.Boolean[A]
      type Number[A] = Self.Schema.Primitive.Number[A]
      type String[A] = Self.Schema.Primitive.String[A]

    type Record[A] = Self.Schema.Record[?, A]

    object Record:
      type Of[S[a] <: Self.Schema[?, a], A] = Self.Schema.Record[S, A]

    type Field[A] = Self.Schema.Field[?, A]

    object Field:
      type Of[S[a] <: Self.Schema[?, a], A] = Self.Schema.Field[S, A]

object SchemaShape extends SchemaShape
