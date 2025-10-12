package io.taig.otter.shape

import io.taig.otter as Self

trait SchemaShape:
  type Schema[A] = Self.Schema[?, A]

  object Schema:
    type Of[S[a] <: Schema[a], A] = Self.Schema[S, A]

    type Coerce[A] = Self.Schema.Coerce[?, A]

    object Coerce:
      type Of[S[a] <: Schema[a], A] = Self.Schema.Coerce[S, A]

    type Constant[A] = Self.Schema.Constant[?, A]

    object Constant:
      type Of[S[a] <: Schema[a], A] = Self.Schema.Constant[S, A]

    type Dictionary[A] = Self.Schema.Dictionary[?, A]

    object Dictionary:
      type Of[S[a] <: Schema[a], A] = Self.Schema.Dictionary[S, A]

    type Enumeration[A] = Self.Schema.Enumeration[?, A]

    object Enumeration:
      type Of[S[a] <: Schema[a], A] = Self.Schema.Enumeration[S, A]

    type Nullable[A] = Self.Schema.Nullable[?, A]

    object Nullable:
      type Of[S[a] <: Schema[a], A] = Self.Schema.Nullable[S, A]

    type Primitive[A] = Self.Schema.Primitive[A]

    object Primitive:
      type Boolean[A] = Self.Schema.Primitive.Boolean[A]

      type Number[A] = Self.Schema.Primitive.Number[A]

      type String[A] = Self.Schema.Primitive.String[A]

    type Record[A] = Self.Schema.Record[?, A]

    object Record:
      type Of[S[a] <: Schema[a], A] = Self.Schema.Record[S, A]

    type Tuple[A] = Self.Schema.Tuple[?, A]

    object Tuple:
      type Of[S[a] <: Schema[a], A] = Self.Schema.Tuple[S, A]

    type Union[A] = Self.Schema.Union[?, A]

    object Union:
      type Of[S[a] <: Schema[a], A] = Self.Schema.Union[S, A]

object SchemaShape extends SchemaShape
