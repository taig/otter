package io.taig.otter.shape

import io.taig.otter as Self

trait TextShape:
  type Text[A] = Self.Text[?, A]

  object Text:
    type Of[S[_], A] = Self.Text[S, A]

    type Coerce[A] = Self.Text.Coerce[?, A]

    object Coerce:
      type Of[S[a] <: Text.Primitive[a], A] = Self.Text.Coerce[S, A]

      export Self.Text.Coerce.unapply

    type Constant[A] = Self.Text.Constant[?, A]

    object Constant:
      type Of[S[a] <: Text[a], A] = Self.Text.Constant[S, A]

      export Self.Text.Constant.unapply

    type Enumeration[A] = Self.Text.Enumeration[?, A]

    object Enumeration:
      type Of[S[a] <: Text[a], A] = Self.Text.Enumeration[S, A]

      export Self.Text.Enumeration.unapply

    type Primitive[A] = Self.Text.Primitive[A]

    object Primitive:
      type Boolean[A] = Self.Text.Primitive.Boolean[A]

      object Boolean:
        export Self.Text.Primitive.Boolean.unapply

      type Number[A] = Self.Text.Primitive.Number[A]

      object Number:
        export Self.Text.Primitive.Number.unapply

      type String[A] = Self.Text.Primitive.String[A]

      object String:
        export Self.Text.Primitive.String.unapply

    type Union[A] = Self.Text.Union[?, A]

    object Union:
      type Of[S[a] <: Text[a], A] = Self.Text.Union[S, A]

      export Self.Text.Union.unapply

object TextShape extends TextShape
