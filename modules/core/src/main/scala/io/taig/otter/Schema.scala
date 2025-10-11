package io.taig.otter

import io.taig.otter as Self
import cats.Invariant
import cats.derived.*
import io.taig.otter.operation.*

sealed abstract class Schema[+S[a] <: Schema[?, a], A] extends Product with Serializable

object Schema:
  final case class Coerce[+S[a] <: Schema[?, a], A](self: Annotation[Self.Coerce[S, A]]) extends Schema[S, A]
      derives Annotated,
        Invariant

  object Coerce:
    given CoerceOperation[Schema[?, *], Schema.Coerce] =
      CoerceOperation[Schema[?, *], [s[a] <: Schema[?, a], a] =>> Annotation[Self.Coerce[s, a]]]
        .imapK[Schema.Coerce]([Value[a] <: Schema[?, a], A] =>
          (self: Annotation[Self.Coerce[Value, A]]) => Coerce(self)
        )([Value[a] <: Schema[?, a], A] => (schema: Coerce[Value, A]) => schema.self)

  final case class Collection[+S[a] <: Schema[?, a], A](self: Annotation[Self.Collection[S, A]]) extends Schema[S, A]
      derives Annotated,
        Invariant

  object Collection:
    given CollectionOperation[Schema[?, *], Schema.Collection] =
      CollectionOperation[Schema[?, *], [s[a] <: Schema[?, a], a] =>> Annotation[Self.Collection[s, a]]]
        .imapK([Value[a] <: Schema[?, a], A] => (self: Annotation[Self.Collection[Value, A]]) => Collection(self))(
          [Value[a] <: Schema[?, a], A] => (schema: Collection[Value, A]) => schema.self
        )

  final case class Constant[+S[a] <: Schema[?, a], A](self: Annotation[Self.Constant[S, A]]) extends Schema[S, A]
      derives Annotated,
        Invariant

  object Constant:
    given ConstantOperation[Schema[?, *], Schema.Constant] =
      ConstantOperation[Schema[?, *], [s[a] <: Schema[?, a], a] =>> Annotation[Self.Constant[s, a]]]
        .imapK[Schema.Constant]([Value[a] <: Schema[?, a], A] =>
          (self: Annotation[Self.Constant[Value, A]]) => Constant(self)
        )([Value[a] <: Schema[?, a], A] => (schema: Constant[Value, A]) => schema.self)

  final case class Dictionary[+S[a] <: Schema[?, a], A](self: Annotation[Self.Dictionary[S, A]]) extends Schema[S, A]
      derives Annotated,
        Invariant

  object Dictionary:
    given DictionaryOperation[Schema[?, *], Schema.Dictionary] =
      DictionaryOperation[Schema[?, *], [s[a] <: Schema[?, a], a] =>> Annotation[Self.Dictionary[s, a]]]
        .imapK([Value[a] <: Schema[?, a], A] => (self: Annotation[Self.Dictionary[Value, A]]) => Dictionary(self))(
          [Value[a] <: Schema[?, a], A] => (schema: Dictionary[Value, A]) => schema.self
        )

  final case class Enumeration[+S[a] <: Schema[?, a], A](self: Annotation[Self.Enumeration[S, A]]) extends Schema[S, A]
      derives Annotated,
        Invariant

  object Enumeration:
    given EnumerationOperation[Schema[?, *], Schema.Enumeration] =
      EnumerationOperation[Schema[?, *], [s[a] <: Schema[?, a], a] =>> Annotation[Self.Enumeration[s, a]]]
        .imapK[Schema.Enumeration]([Value[a] <: Schema[?, a], A] =>
          (self: Annotation[Self.Enumeration[Value, A]]) => Enumeration(self)
        )([Value[a] <: Schema[?, a], A] => (schema: Enumeration[Value, A]) => schema.self)

  sealed abstract class Primitive[A] extends Schema[Nothing, A]

  object Primitive:
    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]]) extends Schema[Nothing, A]
        derives Annotated,
          Invariant

    object Boolean:
      given BooleanOperation[Schema.Primitive.Boolean] =
        BooleanOperation[[a] =>> Annotation[Self.Primitive.Boolean[a]]].mapK([A] =>
          (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self)
        )

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]]) extends Schema[Nothing, A]
        derives Annotated,
          Invariant

    object Number:
      given NumberOperation[Schema.Primitive.Number] =
        NumberOperation[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK([A] =>
          (self: Annotation[Self.Primitive.Number[A]]) => Number(self)
        )([A] => (schema: Schema.Primitive.Number[A]) => schema.self)

    final case class String[A](self: Annotation[Self.Primitive.String[A]]) extends Schema[Nothing, A]
        derives Annotated,
          Invariant

    object String:
      given StringOperation[Schema.Primitive.String] =
        StringOperation[[a] =>> Annotation[Self.Primitive.String[a]]].imapK([A] =>
          (self: Annotation[Self.Primitive.String[A]]) => String(self)
        )([A] => (schema: Schema.Primitive.String[A]) => schema.self)
