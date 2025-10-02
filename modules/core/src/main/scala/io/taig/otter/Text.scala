package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.operation.BooleanOperation
import io.taig.otter.operation.CoerceOperation
import io.taig.otter.operation.NumberOperation
import io.taig.otter.operation.StringOperation

sealed abstract class Text[A] extends Product with Serializable

object Text:
  final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]])

  object Boolean:
    val liftK = [A] => (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self)
    val unliftK = [A] => (schema: Text.Boolean[A]) => schema.self

    given invariant: Invariant[Text.Boolean] =
      Invariant[[a] =>> Annotation[Self.Primitive.Boolean[a]]].imapK(liftK)(unliftK)

    given operation: BooleanOperation[Text.Boolean] =
      BooleanOperation[[a] =>> Annotation[Self.Primitive.Boolean[a]]].mapK(liftK)

  final case class Coerce[A](self: Annotation[Self.Coerce[[a] =>> Text.Boolean[a] | Text.Number[a], A]]) extends Text[A]

  object Coerce:
    val liftK = [A] => (self: Annotation[Self.Coerce[[a] =>> Text.Boolean[a] | Text.Number[a], A]]) => Coerce(self)
    val unliftK = [A] => (schema: Text.Coerce[A]) => schema.self

    given invariant: Invariant[Text.Coerce] =
      Invariant[[a] =>> Annotation[Self.Coerce[[b] =>> Text.Boolean[b] | Text.Number[b], a]]]
        .imapK(liftK)(unliftK)

    given operation: CoerceOperation[Text.Coerce, [a] =>> Text.Boolean[a] | Text.Number[a]] =
      CoerceOperation[
        [a] =>> Annotation[Self.Coerce[[b] =>> Text.Boolean[b] | Text.Number[b], a]],
        [a] =>> Text.Boolean[a] | Text.Number[a]
      ].mapK(liftK)

  final case class Number[A](self: Annotation[Self.Primitive.Number[A]])

  object Number:
    val liftK = [A] => (self: Annotation[Self.Primitive.Number[A]]) => Number(self)
    val unliftK = [A] => (schema: Text.Number[A]) => schema.self

    given invariant: Invariant[Text.Number] =
      Invariant[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK(liftK)(unliftK)

    given operation: NumberOperation[Text.Number] =
      NumberOperation[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK(liftK)(unliftK)

  final case class String[A](self: Annotation[Self.Primitive.String[A]]) extends Text[A]

  object String:
    val liftK = [A] => (self: Annotation[Self.Primitive.String[A]]) => String(self)
    val unliftK = [A] => (schema: Text.String[A]) => schema.self

    given invariant: Invariant[Text.String] =
      Invariant[[a] =>> Annotation[Self.Primitive.String[a]]].imapK(liftK)(unliftK)

    given operation: StringOperation[Text.String] =
      StringOperation[[a] =>> Annotation[Self.Primitive.String[a]]].imapK(liftK)(unliftK)

  given invariant: Invariant[Text] with
    extension [A](self: Text[A])
      override def imap[B](f: A => B)(g: B => A): Text[B] = self match
        case Text.Coerce(self) => Text.Coerce(self.imap(f)(g))
        case Text.String(self) => Text.String(self.imap(f)(g))
