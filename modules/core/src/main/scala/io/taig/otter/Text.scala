package io.taig.otter

import io.taig.otter.operation.*
import io.taig.otter as Self
import cats.Invariant
import cats.derived.*
import cats.syntax.all.*
import cats.kernel.Eq

sealed abstract class Text[A] extends Product with Serializable derives Invariant

object Text:
  sealed trait Primitive[A] extends Product with Serializable derives Invariant

  object Primitive:
    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]]) extends Text.Primitive[A]
        derives Annotated,
          Invariant

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]]) extends Text.Primitive[A]
        derives Annotated,
          Invariant

    final case class String[A](self: Annotation[Self.Primitive.String[A]]) extends Text[A], Text.Primitive[A]
        derives Annotated,
          Invariant

    given annotated: Annotated[Text.Primitive] with
      override def get[A](self: Primitive[A]): Metadata = self match
        case schema: Text.Primitive.Boolean[?] => Annotated[Text.Primitive.Boolean].get(schema)
        case schema: Text.Primitive.Number[?]  => Annotated[Text.Primitive.Number].get(schema)
        case schema: Text.Primitive.String[?]  => Annotated[Text.Primitive.String].get(schema)

      override def update[A](self: Primitive[A], metadata: Metadata => Metadata): Primitive[A] = self match
        case schema: Text.Primitive.Boolean[?] => Annotated[Text.Primitive.Boolean].update(schema, metadata)
        case schema: Text.Primitive.Number[?]  => Annotated[Text.Primitive.Number].update(schema, metadata)
        case schema: Text.Primitive.String[?]  => Annotated[Text.Primitive.String].update(schema, metadata)

  final case class Coerce[A](self: Annotation[Self.Coerce[Text.Primitive, A]]) extends Text[A]
      derives Annotated,
        Invariant

  final case class Constant[A](self: Annotation[Self.Constant[Text, A]]) extends Text[A] derives Annotated, Invariant

  final case class Enumeration[A](self: Annotation[Self.Enumeration[Text, A]]) extends Text[A]
      derives Annotated,
        Invariant

  final case class Union[A](self: Annotation[Self.Union[Text, A]]) extends Text[A] derives Annotated, Invariant
