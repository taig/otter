package io.taig.otter.shape

import io.taig.otter as Self

trait SchemaShape:
  type Schema[A] = Self.Schema[?, A]

  object Schema:
    type Of[+S[a] <: Schema[a], A] = Self.Schema[S, A]

    type Read[+A] = Self.Schema.Read[?, A]

    object Read:
      type Of[+S[a] <: Schema.Read[a], A] = Self.Schema.Read[S, A]

    type Write[-A] = Self.Schema.Write[?, A]

    object Write:
      type Of[+S[a] <: Schema.Write[a], A] = Self.Schema.Write[S, A]

    type Collection[A] = Self.Schema.Collection[?, A]

    object Collection:
      type Of[+S[a] <: Schema[a], A] = Self.Schema.Collection[S, A]

      type Read[+A] = Self.Schema.Collection.Read[?, A]

      object Read:
        type Of[+S[a] <: Schema.Read[a], A] = Self.Schema.Collection.Read[S, A]

      type Write[-A] = Self.Schema.Collection.Write[?, A]

      object Write:
        type Of[+S[a] <: Schema.Write[a], A] = Self.Schema.Collection.Write[S, A]

    type Dictionary[A] = Self.Schema.Dictionary[?, A]

    object Dictionary:
      type Of[+S[a] <: Schema[a], A] = Self.Schema.Dictionary[S, A]

      type Read[+A] = Self.Schema.Dictionary.Read[?, A]

      object Read:
        type Of[+S[a] <: Schema.Read[a], A] = Self.Schema.Dictionary.Read[S, A]

      type Write[-A] = Self.Schema.Dictionary.Write[?, A]

      object Write:
        type Of[+S[a] <: Schema.Write[a], A] = Self.Schema.Dictionary.Write[S, A]

object SchemaShape extends SchemaShape
