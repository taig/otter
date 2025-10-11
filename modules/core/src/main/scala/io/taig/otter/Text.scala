package io.taig.otter

import io.taig.otter.operation.*
import io.taig.otter as Self
import cats.Invariant
import cats.derived.*
import cats.syntax.all.*
import cats.kernel.Eq

sealed abstract class Text[+S[_], A] extends Product with Serializable derives Invariant

object Text:
  sealed trait Primitive[A] extends Product with Serializable derives Invariant

  object Primitive:
    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]]) extends Text.Primitive[A]
        derives Annotated,
          Invariant

    object Boolean:
      given BooleanOperation[Text.Primitive.Boolean] =
        BooleanOperation[[a] =>> Annotation[Self.Primitive.Boolean[a]]].mapK([A] =>
          (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self)
        )

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]]) extends Text.Primitive[A]
        derives Annotated,
          Invariant

    object Number:
      given NumberOperation[Text.Primitive.Number] =
        NumberOperation[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK([A] =>
          (self: Annotation[Self.Primitive.Number[A]]) => Number(self)
        )([A] => (schema: Number[A]) => schema.self)

    final case class String[A](self: Annotation[Self.Primitive.String[A]]) extends Text[Nothing, A], Text.Primitive[A]
        derives Annotated,
          Invariant

    object String:
      given StringOperation[Text.Primitive.String] =
        StringOperation[[a] =>> Annotation[Self.Primitive.String[a]]].imapK([A] =>
          (self: Annotation[Self.Primitive.String[A]]) => String(self)
        )([A] => (schema: String[A]) => schema.self)

    given annotated: Annotated[Text.Primitive] with
      override def get[A](self: Primitive[A]): Metadata = self match
        case schema: Text.Primitive.Boolean[?] => Annotated[Text.Primitive.Boolean].get(schema)
        case schema: Text.Primitive.Number[?]  => Annotated[Text.Primitive.Number].get(schema)
        case schema: Text.Primitive.String[?]  => Annotated[Text.Primitive.String].get(schema)

      override def update[A](self: Primitive[A], metadata: Metadata => Metadata): Primitive[A] = self match
        case schema: Text.Primitive.Boolean[?] => Annotated[Text.Primitive.Boolean].update(schema, metadata)
        case schema: Text.Primitive.Number[?]  => Annotated[Text.Primitive.Number].update(schema, metadata)
        case schema: Text.Primitive.String[?]  => Annotated[Text.Primitive.String].update(schema, metadata)

  final case class Coerce[+S[a] <: Text.Primitive[a], A](self: Annotation[Self.Coerce[S, A]]) extends Text[S, A]
      derives Annotated,
        Invariant

  object Coerce:
    given CoerceOperation[Text.Primitive, Text.Coerce] =
      CoerceOperation[Text.Primitive, [s[a] <: Text.Primitive[a], a] =>> Annotation[Self.Coerce[s, a]]]
        .imapK[Text.Coerce]([Value[a] <: Text.Primitive[a], A] =>
          (self: Annotation[Self.Coerce[Value, A]]) => Coerce(self)
        )([Value[a] <: Text.Primitive[a], A] => (schema: Text.Coerce[Value, A]) => schema.self)

  final case class Constant[+S[a] <: Text[?, a], A](self: Annotation[Self.Constant[S, A]]) extends Text[S, A]
      derives Annotated,
        Invariant

  object Constant:
    given ConstantOperation[Text[?, *], Text.Constant] =
      ConstantOperation[Text[?, *], [s[a] <: Text[?, a], a] =>> Annotation[Self.Constant[s, a]]]
        .imapK[Text.Constant]([Value[a] <: Text[?, a], A] =>
          (self: Annotation[Self.Constant[Value, A]]) => Constant(self)
        )([Value[a] <: Text[?, a], A] => (schema: Text.Constant[Value, A]) => schema.self)

  final case class Enumeration[+S[a] <: Text[?, a], A](self: Annotation[Self.Enumeration[S, A]]) extends Text[S, A]
      derives Annotated,
        Invariant

  object Enumeration:
    given EnumerationOperation[Text[?, *], Text.Enumeration] =
      EnumerationOperation[Text[?, *], [s[a] <: Text[?, a], a] =>> Annotation[Self.Enumeration[s, a]]]
        .imapK[Text.Enumeration]([Value[a] <: Text[?, a], A] =>
          (self: Annotation[Self.Enumeration[Value, A]]) => Enumeration(self)
        )([Value[a] <: Text[?, a], A] => (schema: Enumeration[Value, A]) => schema.self)

  final case class Union[+S[a] <: Text[?, a], A](self: Annotation[Self.Union[S, A]]) extends Text[S, A]
      derives Annotated,
        Invariant

  object Union:
    given UnionOperation[Text[?, *], Text.Union] =
      UnionOperation[Text[?, *], [s[a] <: Text[?, a], a] =>> Annotation[Self.Union[s, a]]]
        .imapK[Text.Union]([Value[a] <: Text[?, a], A] => (self: Annotation[Self.Union[Value, A]]) => Union(self))(
          [Value[a] <: Text[?, a], A] => (schema: Union[Value, A]) => schema.self
        )
