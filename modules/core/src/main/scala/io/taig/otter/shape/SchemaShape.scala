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

    type Nullish[A] = Self.Schema.Nullish[?, A]

    object Nullish:
      type Of[+S[a] <: Schema[a], A] = Self.Schema.Nullish[S, A]
      export Self.Schema.Nullish.{apply, unapply}

      type Read[+A] = Self.Schema.Nullish.Read[?, A]

      object Read:
        type Of[+S[a] <: Schema.Read[a], A] = Self.Schema.Nullish.Read[S, A]
        export Self.Schema.Nullish.Read.{apply, unapply}

      type Write[-A] = Self.Schema.Nullish.Write[?, A]

      object Write:
        type Of[+S[a] <: Schema.Write[a], A] = Self.Schema.Nullish.Write[S, A]
        export Self.Schema.Nullish.Write.{apply, unapply}

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

    type Record[A] = Self.Schema.Record[?, A]

    object Record:
      type Of[+S[a] <: Schema[a], A] = Self.Schema.Record[S, A]
      export Self.Schema.Record.{apply, unapply}

      type Read[+A] = Self.Schema.Record.Read[?, A]

      object Read:
        type Of[+S[a] <: Schema.Read[a], A] = Self.Schema.Record.Read[S, A]
        export Self.Schema.Record.Read.{apply, unapply}

      type Write[-A] = Self.Schema.Record.Write[?, A]

      object Write:
        type Of[+S[a] <: Schema.Write[a], A] = Self.Schema.Record.Write[S, A]
        export Self.Schema.Record.Write.{apply, unapply}

    type Tuple[A] = Self.Schema.Tuple[?, A]

    object Tuple:
      type Of[+S[a] <: Schema[a], A] = Self.Schema.Tuple[S, A]
      export Self.Schema.Tuple.{apply, unapply}

      type Read[+A] = Self.Schema.Tuple.Read[?, A]

      object Read:
        type Of[+S[a] <: Schema.Read[a], A] = Self.Schema.Tuple.Read[S, A]
        export Self.Schema.Tuple.Read.{apply, unapply}

      type Write[-A] = Self.Schema.Tuple.Write[?, A]

      object Write:
        type Of[+S[a] <: Schema.Write[a], A] = Self.Schema.Tuple.Write[S, A]
        export Self.Schema.Tuple.Write.{apply, unapply}

    type Union[A] = Self.Schema.Union[?, A]

    object Union:
      type Of[+S[a] <: Schema[a], A] = Self.Schema.Union[S, A]
      export Self.Schema.Union.{apply, unapply}

      type Read[+A] = Self.Schema.Union.Read[?, A]

      object Read:
        type Of[+S[a] <: Schema.Read[a], A] = Self.Schema.Union.Read[S, A]
        export Self.Schema.Union.Read.{apply, unapply}

      type Write[-A] = Self.Schema.Union.Write[?, A]

      object Write:
        type Of[+S[a] <: Schema.Write[a], A] = Self.Schema.Union.Write[S, A]
        export Self.Schema.Union.Write.{apply, unapply}

    type Field[A] = Self.Schema.Field[?, A]

    object Field:
      type Of[+S[a] <: Schema[a], A] = Self.Schema.Field[S, A]
      export Self.Schema.Field.{apply, unapply}

      type Read[+A] = Self.Schema.Field.Read[?, A]

      object Read:
        type Of[+S[a] <: Schema.Read[a], A] = Self.Schema.Field.Read[S, A]
        export Self.Schema.Field.Read.{apply, unapply}

      type Write[-A] = Self.Schema.Field.Write[?, A]

      object Write:
        type Of[+S[a] <: Schema.Write[a], A] = Self.Schema.Field.Write[S, A]
        export Self.Schema.Field.Write.{apply, unapply}

object SchemaShape extends SchemaShape
