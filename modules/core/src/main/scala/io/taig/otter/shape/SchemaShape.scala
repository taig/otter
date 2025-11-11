package io.taig.otter.shape

import io.taig.otter as Self

trait SchemaShape:
  type Schema[A] = Self.Schema[?, A]

  object Schema:
    type Of[+S[a] <: Schema[a], A] = Self.Schema[S, A]
    export Self.Schema.{apply, unapply}

    type Read[+A] = Self.Schema.Read[?, A]

    object Read:
      type Of[+S[a] <: Schema.Read[a], A] = Self.Schema.Read[S, A]
      export Self.Schema.Read.{apply, unapply}

    type Write[-A] = Self.Schema.Write[?, A]

    object Write:
      type Of[+S[a] <: Schema.Write[a], A] = Self.Schema.Write[S, A]
      export Self.Schema.Write.{apply, unapply}

    type Coerce[A] = Self.Schema.Coerce[?, A]

    object Coerce:
      type Of[+S[a] <: Schema[a], A] = Self.Schema.Coerce[S, A]
      export Self.Schema.Coerce.{apply, unapply}

      type Read[+A] = Self.Schema.Coerce.Read[?, A]

      object Read:
        type Of[+S[a] <: Schema.Read[a], A] = Self.Schema.Coerce.Read[S, A]
        export Self.Schema.Coerce.Read.{apply, unapply}

      type Write[-A] = Self.Schema.Coerce.Write[?, A]

      object Write:
        type Of[+S[a] <: Schema.Write[a], A] = Self.Schema.Coerce.Write[S, A]
        export Self.Schema.Coerce.Write.{apply, unapply}

    type Collection[A] = Self.Schema.Collection[?, A]

    object Collection:
      type Of[+S[a] <: Schema[a], A] = Self.Schema.Collection[S, A]
      export Self.Schema.Collection.{apply, unapply}

      type Read[+A] = Self.Schema.Collection.Read[?, A]

      object Read:
        type Of[+S[a] <: Schema.Read[a], A] = Self.Schema.Collection.Read[S, A]
        export Self.Schema.Collection.Read.{apply, unapply}

      type Write[-A] = Self.Schema.Collection.Write[?, A]

      object Write:
        type Of[+S[a] <: Schema.Write[a], A] = Self.Schema.Collection.Write[S, A]
        export Self.Schema.Collection.Write.{apply, unapply}

    type Constant[A] = Self.Schema.Constant[?, A]

    object Constant:
      type Of[+S[a] <: Schema[a], A] = Self.Schema.Constant[S, A]
      export Self.Schema.Constant.{apply, unapply}

      type Read[+A] = Self.Schema.Constant.Read[?, A]

      object Read:
        type Of[+S[a] <: Schema.Read[a], A] = Self.Schema.Constant.Read[S, A]
        export Self.Schema.Constant.Read.{apply, unapply}

      type Write[-A] = Self.Schema.Constant.Write[?, A]

      object Write:
        type Of[+S[a] <: Schema.Write[a], A] = Self.Schema.Constant.Write[S, A]
        export Self.Schema.Constant.Write.{apply, unapply}

    type Dictionary[A] = Self.Schema.Dictionary[?, A]

    object Dictionary:
      type Of[+S[a] <: Schema[a], A] = Self.Schema.Dictionary[S, A]
      export Self.Schema.Dictionary.{apply, unapply}

      type Read[+A] = Self.Schema.Dictionary.Read[?, A]

      object Read:
        type Of[+S[a] <: Schema.Read[a], A] = Self.Schema.Dictionary.Read[S, A]
        export Self.Schema.Dictionary.Read.{apply, unapply}

      type Write[-A] = Self.Schema.Dictionary.Write[?, A]

      object Write:
        type Of[+S[a] <: Schema.Write[a], A] = Self.Schema.Dictionary.Write[S, A]
        export Self.Schema.Dictionary.Write.{apply, unapply}

    type Enumeration[A] = Self.Schema.Enumeration[?, A]

    object Enumeration:
      type Of[+S[a] <: Schema[a], A] = Self.Schema.Enumeration[S, A]
      export Self.Schema.Enumeration.{apply, unapply}

      type Read[+A] = Self.Schema.Enumeration.Read[?, A]

      object Read:
        type Of[+S[a] <: Schema.Read[a], A] = Self.Schema.Enumeration.Read[S, A]
        export Self.Schema.Enumeration.Read.{apply, unapply}

      type Write[-A] = Self.Schema.Enumeration.Write[?, A]

      object Write:
        type Of[+S[a] <: Schema.Write[a], A] = Self.Schema.Enumeration.Write[S, A]
        export Self.Schema.Enumeration.Write.{apply, unapply}

    type Nullable[A] = Self.Schema.Nullable[?, A]

    object Nullable:
      type Of[+S[a] <: Schema[a], A] = Self.Schema.Nullable[S, A]
      export Self.Schema.Nullable.{apply, unapply}

      type Read[+A] = Self.Schema.Nullable.Read[?, A]

      object Read:
        type Of[+S[a] <: Schema.Read[a], A] = Self.Schema.Nullable.Read[S, A]
        export Self.Schema.Nullable.Read.{apply, unapply}

      type Write[-A] = Self.Schema.Nullable.Write[?, A]

      object Write:
        type Of[+S[a] <: Schema.Write[a], A] = Self.Schema.Nullable.Write[S, A]
        export Self.Schema.Nullable.Write.{apply, unapply}

    type Primitive[A] = Self.Schema.Primitive[A]

    object Primitive:
      export Self.Schema.Primitive.{apply, unapply}

      type Read[+A] = Self.Schema.Primitive.Read[A]

      object Read:
        export Self.Schema.Primitive.Read.{apply, unapply}

      type Write[-A] = Self.Schema.Primitive.Write[A]

      object Write:
        export Self.Schema.Primitive.Write.{apply, unapply}

      type Boolean[A] = Self.Schema.Primitive.Boolean[A]

      object Boolean:
        export Self.Schema.Primitive.Boolean.{apply, unapply}

        type Read[+A] = Self.Schema.Primitive.Boolean.Read[A]

        object Read:
          export Self.Schema.Primitive.Boolean.Read.{apply, unapply}

        type Write[-A] = Self.Schema.Primitive.Boolean.Write[A]

        object Write:
          export Self.Schema.Primitive.Boolean.Write.{apply, unapply}

      type Number[A] = Self.Schema.Primitive.Number[A]

      object Number:
        export Self.Schema.Primitive.Number.{apply, unapply}

        type Read[+A] = Self.Schema.Primitive.Number.Read[A]

        object Read:
          export Self.Schema.Primitive.Number.Read.{apply, unapply}

        type Write[-A] = Self.Schema.Primitive.Number.Write[A]

        object Write:
          export Self.Schema.Primitive.Number.Write.{apply, unapply}

      type Text[A] = Self.Schema.Primitive.Text[A]

      object Text:
        export Self.Schema.Primitive.Text.{apply, unapply}

        type Read[+A] = Self.Schema.Primitive.Text.Read[A]

        object Read:
          export Self.Schema.Primitive.Text.Read.{apply, unapply}

        type Write[-A] = Self.Schema.Primitive.Text.Write[A]

        object Write:
          export Self.Schema.Primitive.Text.Write.{apply, unapply}

object SchemaShape extends SchemaShape
