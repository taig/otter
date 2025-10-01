package io.taig.otter

import io.taig.otter as Self

sealed abstract class Text[A] extends Product with Serializable

object Text:
  final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]])

  // object Boolean:
  //   given BooleanSchemaInvariant[Text.Boolean] =
  //     val fK = [A] => (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self)
  //     val gK = [A] => (schema: Text.Boolean[A]) => schema.self
  //     BooleanSchemaInvariant[[a] =>> Annotation[Self.Primitive.Boolean[a]]].imapK(fK)(gK)

  final case class Coerce[A](self: Annotation[Text.Boolean[A] | Text.Number[A]]) extends Text[A]

  final case class String[A](self: Annotation[Self.Primitive.String[A]]) extends Text[A]

  // object String:
  //   given StringSchemaInvariant[Text.String, Constraint.Primitive.Text] =
  //     val fK = [A] => (self: Annotation[Self.Primitive.String[A]]) => String(self)
  //     val gK = [A] => (schema: Text.String[A]) => schema.self
  //     StringSchemaInvariant[[a] =>> Annotation[Self.Primitive.String[a]], Constraint.Primitive.Text].imapK(fK)(gK)

  final case class Number[A](self: Annotation[Self.Primitive.Number[A]]) extends Text[A]

  // object Number:
  //   given NumberSchemaInvariant[Text.Number, Constraint.Primitive.Number] =
  //     val fK = [A] => (self: Annotation[Self.Primitive.Number[A]]) => Number(self)
  //     val gK = [A] => (schema: Text.Number[A]) => schema.self
  //     NumberSchemaInvariant[[a] =>> Annotation[Self.Primitive.Number[a]], Constraint.Primitive.Number].imapK(fK)(gK)
