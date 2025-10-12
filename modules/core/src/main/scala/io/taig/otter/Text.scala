package io.taig.otter

import io.taig.otter.operation.*
import io.taig.otter as Self
import cats.Invariant
import cats.derived.*
import cats.syntax.all.*

sealed abstract class Text[+S[_], A] extends Product with Serializable derives Invariant

object Text:
  final case class Coerce[+S[a] <: Text.Primitive[a], A](self: Annotation[Self.Coerce[S, A]]) extends Text[S, A]
      derives Invariant

  object Coerce:
    given [S[a] <: Text.Primitive[a], A]: Annotated[Text.Coerce[S, A]] =
      Annotated[Annotation[Self.Coerce[S, A]]].imap(Coerce.apply)(_.self)

    given CoerceOperation[Text.Primitive, Text.Coerce] =
      CoerceOperation[Text.Primitive, [s[a] <: Text.Primitive[a], a] =>> Annotation[Self.Coerce[s, a]]]
        .imapK[Text.Coerce]([Value[a] <: Text.Primitive[a], A] =>
          (self: Annotation[Self.Coerce[Value, A]]) => Coerce(self)
        )([Value[a] <: Text.Primitive[a], A] => (schema: Text.Coerce[Value, A]) => schema.self)

  final case class Constant[+S[a] <: Text[?, a], A](self: Annotation[Self.Constant[S, A]]) extends Text[S, A]
      derives Invariant

  object Constant:
    given [S[a] <: Text[?, a], A]: Annotated[Text.Constant[S, A]] =
      Annotated[Annotation[Self.Constant[S, A]]].imap(Constant.apply)(_.self)

    given ConstantOperation[Text[?, *], Text.Constant] =
      ConstantOperation[Text[?, *], [s[a] <: Text[?, a], a] =>> Annotation[Self.Constant[s, a]]]
        .imapK[Text.Constant]([Value[a] <: Text[?, a], A] =>
          (self: Annotation[Self.Constant[Value, A]]) => Constant(self)
        )([Value[a] <: Text[?, a], A] => (schema: Text.Constant[Value, A]) => schema.self)

  final case class Enumeration[+S[a] <: Text[?, a], A](self: Annotation[Self.Enumeration[S, A]]) extends Text[S, A]
      derives Invariant

  object Enumeration:
    given [S[a] <: Text[?, a], A]: Annotated[Text.Enumeration[S, A]] =
      Annotated[Annotation[Self.Enumeration[S, A]]].imap(Enumeration.apply)(_.self)

    given EnumerationOperation[Text[?, *], Text.Enumeration] =
      EnumerationOperation[Text[?, *], [s[a] <: Text[?, a], a] =>> Annotation[Self.Enumeration[s, a]]]
        .imapK[Text.Enumeration]([Value[a] <: Text[?, a], A] =>
          (self: Annotation[Self.Enumeration[Value, A]]) => Enumeration(self)
        )([Value[a] <: Text[?, a], A] => (schema: Enumeration[Value, A]) => schema.self)

  sealed trait Primitive[A] extends Product with Serializable derives Invariant

  object Primitive:
    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]]) extends Text.Primitive[A] derives Invariant

    object Boolean:
      given [A]: Annotated[Text.Primitive.Boolean[A]] = Annotated[Annotation[Self.Primitive.Boolean[A]]]
        .imap(Boolean.apply)(_.self)

      given BooleanOperation[Text.Primitive.Boolean] =
        BooleanOperation[[a] =>> Annotation[Self.Primitive.Boolean[a]]].mapK([A] =>
          (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self)
        )

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]]) extends Text.Primitive[A] derives Invariant

    object Number:
      given [A]: Annotated[Text.Primitive.Number[A]] = Annotated[Annotation[Self.Primitive.Number[A]]]
        .imap(Number.apply)(_.self)

      given NumberOperation[Text.Primitive.Number] =
        NumberOperation[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK([A] =>
          (self: Annotation[Self.Primitive.Number[A]]) => Number(self)
        )([A] => (schema: Number[A]) => schema.self)

    final case class String[A](self: Annotation[Self.Primitive.String[A]]) extends Text[Nothing, A], Text.Primitive[A]
        derives Invariant

    object String:
      given [A]: Annotated[Text.Primitive.String[A]] = Annotated[Annotation[Self.Primitive.String[A]]]
        .imap(String.apply)(_.self)

      given StringOperation[Text.Primitive.String] =
        StringOperation[[a] =>> Annotation[Self.Primitive.String[a]]].imapK([A] =>
          (self: Annotation[Self.Primitive.String[A]]) => String(self)
        )([A] => (schema: String[A]) => schema.self)

    given [A]: Annotated[Text.Primitive[A]] = Annotated[Annotation[Self.Primitive[A]]]
      .imap { self =>
        self.self match
          case schema: Self.Primitive.Boolean[A] => Boolean(self.copy(self = schema))
          case schema: Self.Primitive.Number[A]  => Number(self.copy(self = schema))
          case schema: Self.Primitive.String[A]  => String(self.copy(self = schema))
      } {
        case Text.Primitive.Boolean(self) => self
        case Text.Primitive.Number(self)  => self
        case Text.Primitive.String(self)  => self
      }

    given PrimitiveOperation[Text.Primitive] =
      PrimitiveOperation[[a] =>> Annotation[Self.Primitive[a]]].imapK([A] =>
        (self: Annotation[Self.Primitive[A]]) =>
          self.self match
            case schema: Self.Primitive.Boolean[A] => Text.Primitive.Boolean(self.copy(self = schema))
            case schema: Self.Primitive.Number[A]  => Text.Primitive.Number(self.copy(self = schema))
            case schema: Self.Primitive.String[A]  => Text.Primitive.String(self.copy(self = schema))
      )([A] =>
        (schema: Text.Primitive[A]) =>
          schema match
            case Text.Primitive.Boolean(self) => self
            case Text.Primitive.Number(self)  => self
            case Text.Primitive.String(self)  => self
      )

  final case class Union[+S[a] <: Text[?, a], A](self: Annotation[Self.Union[S, A]]) extends Text[S, A]
      derives Invariant

  object Union:
    given [S[a] <: Text[?, a], A]: Annotated[Text.Union[S, A]] =
      Annotated[Annotation[Self.Union[S, A]]].imap(Union.apply)(_.self)

    given UnionOperation[Text[?, *], Text.Union] =
      UnionOperation[Text[?, *], [s[a] <: Text[?, a], a] =>> Annotation[Self.Union[s, a]]]
        .imapK[Text.Union]([Value[a] <: Text[?, a], A] => (self: Annotation[Self.Union[Value, A]]) => Union(self))(
          [Value[a] <: Text[?, a], A] => (schema: Union[Value, A]) => schema.self
        )
