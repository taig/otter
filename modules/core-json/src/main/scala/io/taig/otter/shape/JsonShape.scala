package io.taig.otter.shape

import io.taig.otter as Self

trait JsonShape:
  type Json[A] = Self.Json[?, A]

  object Json:
    type Of[S[a] <: Self.Json[?, a], A] = Self.Json[S, A]

    type Coerce[A] = Self.Json.Coerce[A]

    object Coerce:
      export Self.Json.Coerce.unapply

    type Collection[A] = Self.Json.Collection[?, A]

    object Collection:
      type Of[S[a] <: Self.Json[?, a], A] = Self.Json.Collection[S, A]

      export Self.Json.Collection.unapply

    type Constant[A] = Self.Json.Constant[A]

    object Constant:
      export Self.Json.Constant.unapply

    type Dictionary[A] = Self.Json.Dictionary[?, A]

    object Dictionary:
      type Of[S[a] <: Self.Json[?, a], A] = Self.Json.Dictionary[S, A]

      export Self.Json.Dictionary.unapply

    type Nullable[A] = Self.Json.Nullable[?, A]

    object Nullable:
      type Of[S[a] <: Self.Json[?, a], A] = Self.Json.Nullable[S, A]

      export Self.Json.Nullable.unapply

    type Primitive[A] = Self.Json.Primitive[A]

    object Primitive:
      export Self.Json.Primitive.unapply

    type Record[A] = Self.Json.Record[?, A]

    object Record:
      type Of[S[a] <: Self.Json[?, a], A] = Self.Json.Record[S, A]

      export Self.Json.Record.unapply

    type Field[A] = Self.Json.Field[?, A]

    object Field:
      type Of[S[a] <: Self.Json[?, a], A] = Self.Json.Field[S, A]

object JsonShape extends JsonShape
