package io.taig.otter.shape

import io.taig.otter as Self

trait SchemaShape:
  type Schema[A] = Self.Schema[?, A]

  object Schema:
    type Of[S[a] <: Self.Schema[?, a], A] = Self.Schema[S, A]

    type Collection[A] = Self.Schema.Collection[?, A]

    object Collection:
      type Of[S[a] <: Self.Schema[?, a], A] = Self.Schema.Collection[S, A]

      export Self.Schema.Collection.unapply

    type Constant[A] = Self.Schema.Constant[?, A]

    object Constant:
      type Of[S[a] <: Self.Schema[?, a], A] = Self.Schema.Constant[S, A]

      export Self.Schema.Constant.unapply

    type Dictionary[A] = Self.Schema.Dictionary[?, A]

    object Dictionary:
      type Of[S[a] <: Self.Schema[?, a], A] = Self.Schema.Dictionary[S, A]

      export Self.Schema.Dictionary.unapply

    type Enumeration[A] = Self.Schema.Enumeration[?, A]

    object Enumeration:
      type Of[S[a] <: Self.Schema[?, a], A] = Self.Schema.Enumeration[S, A]

      export Self.Schema.Enumeration.unapply

    type Primitive[A] = Self.Schema.Primitive[A]

    object Primitive:
      type Boolean[A] = Self.Schema.Primitive.Boolean[A]

      object Boolean:
        export Self.Schema.Primitive.Boolean.unapply

      type Number[A] = Self.Schema.Primitive.Number[A]

      object Number:
        export Self.Schema.Primitive.Number.unapply

      type String[A] = Self.Schema.Primitive.String[A]

      object String:
        export Self.Schema.Primitive.String.unapply

    type Record[A] = Self.Schema.Record[?, A]

    object Record:
      type Of[S[a] <: Self.Schema[?, a], A] = Self.Schema.Record[S, A]

      export Self.Schema.Record.unapply

    type Tuple[A] = Self.Schema.Tuple[?, A]

    object Tuple:
      type Of[S[a] <: Self.Schema[?, a], A] = Self.Schema.Tuple[S, A]

      export Self.Schema.Tuple.unapply

    type Field[A] = Self.Schema.Field[?, A]

    object Field:
      type Of[S[a] <: Self.Schema[?, a], A] = Self.Schema.Field[S, A]

      export Self.Schema.Field.unapply

object SchemaShape extends SchemaShape
