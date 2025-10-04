package io.taig.otter

import io.taig.otter as Self
import io.taig.otter.operation.BooleanOperation
import io.taig.otter.operation.CoerceOperation
import io.taig.otter.operation.ConstantOperation
import io.taig.otter.operation.EnumerationOperation
import io.taig.otter.operation.NumberOperation
import io.taig.otter.operation.StringOperation
import io.taig.otter.operation.UnionOperation

sealed abstract class Text[+S[a] <: Text[?, a], A] extends Product with Serializable

object Text:
  final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]])

  object Boolean:
    val liftK = [A] => (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self)
    val unliftK = [A] => (schema: Text.Boolean[A]) => schema.self

    given invariant: Invariant[Text.Boolean] =
      Invariant[[a] =>> Annotation[Self.Primitive.Boolean[a]]].imapK(liftK)(unliftK)

    given operation: BooleanOperation[Text.Boolean] =
      BooleanOperation[[a] =>> Annotation[Self.Primitive.Boolean[a]]].mapK(liftK)

  final case class Coerce[A](self: Annotation[Self.Coerce[[a] =>> Text.Boolean[a] | Text.Number[a], A]])
      extends Text[Nothing, A]

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

  final case class Constant[+S[a] <: Text[?, a], A](self: Annotation[Self.Constant[S, A]]) extends Text[S, A]

  object Constant:
    def liftK[S[a] <: Text[?, a]] = [A] => (self: Annotation[Self.Constant[S, A]]) => Constant(self)
    def unliftK[S[a] <: Text[?, a]] = [A] => (schema: Text.Constant[S, A]) => schema.self

    given invariant[S[a] <: Text[?, a]]: Invariant[Text.Constant[S, *]] =
      Invariant[[a] =>> Annotation[Self.Constant[S, a]]].imapK(liftK[S])(unliftK[S])

    given operation[S[a] <: Text[?, a]]: ConstantOperation[Text.Constant[S, *], S] =
      ConstantOperation[[a] =>> Annotation[Self.Constant[S, a]], S].mapK(liftK[S])

  final case class Enumeration[+S[a] <: Text[?, a], A](self: Annotation[Self.Enumeration[S, A]]) extends Text[S, A]

  object Enumeration:
    def liftK[S[a] <: Text[?, a]] = [A] => (self: Annotation[Self.Enumeration[S, A]]) => Enumeration(self)
    def unliftK[S[a] <: Text[?, a]] = [A] => (schema: Text.Enumeration[S, A]) => schema.self

    given invariant[S[a] <: Text[?, a]]: Invariant[Text.Enumeration[S, *]] =
      Invariant[[a] =>> Annotation[Self.Enumeration[S, a]]].imapK(liftK[S])(unliftK[S])

    given operation[S[a] <: Text[?, a]]: EnumerationOperation[Text.Enumeration[S, *], S] =
      EnumerationOperation[[a] =>> Annotation[Self.Enumeration[S, a]], S].mapK(liftK[S])

  final case class Number[A](self: Annotation[Self.Primitive.Number[A]])

  object Number:
    val liftK = [A] => (self: Annotation[Self.Primitive.Number[A]]) => Number(self)
    val unliftK = [A] => (schema: Text.Number[A]) => schema.self

    given invariant: Invariant[Text.Number] =
      Invariant[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK(liftK)(unliftK)

    given operation: NumberOperation[Text.Number] =
      NumberOperation[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK(liftK)(unliftK)

  final case class String[A](self: Annotation[Self.Primitive.String[A]]) extends Text[Nothing, A]

  object String:
    val liftK = [A] => (self: Annotation[Self.Primitive.String[A]]) => String(self)
    val unliftK = [A] => (schema: Text.String[A]) => schema.self

    given invariant: Invariant[Text.String] =
      Invariant[[a] =>> Annotation[Self.Primitive.String[a]]].imapK(liftK)(unliftK)

    given operation: StringOperation[Text.String] =
      StringOperation[[a] =>> Annotation[Self.Primitive.String[a]]].imapK(liftK)(unliftK)

  final case class Union[+S[a] <: Text[?, a], A](self: Annotation[Self.Union[S, A]]) extends Text[S, A]

  object Union:
    def liftK[S[a] <: Text[?, a]] = [A] => (self: Annotation[Self.Union[S, A]]) => Union(self)
    def unliftK[S[a] <: Text[?, a]] = [A] => (schema: Text.Union[S, A]) => schema.self

    given invariant[S[a] <: Text[?, a]]: Invariant[Text.Union[S, *]] =
      Invariant[[a] =>> Annotation[Self.Union[S, a]]].imapK(liftK[S])(unliftK[S])

    given operation[S[a] <: Text[?, a]]: UnionOperation[Text.Union[S, *], S] =
      UnionOperation[[a] =>> Annotation[Self.Union[S, a]], S].mapK(liftK[S])

  given invariant[S[a] <: Text[?, a]]: Invariant[Text[S, *]] with
    extension [A](self: Text[S, A])
      override def imap[B](f: A => B)(g: B => A): Text[S, B] = self match
        case self: Text.Coerce[?]      => self.imap(f)(g)
        case self: Text.Constant[?, ?] => self.imap(f)(g)
        case self: Text.Enumeration[?, ?] => self.imap(f)(g)
        case self: Text.String[?]      => self.imap(f)(g)
        case self: Text.Union[?, ?]    => self.imap(f)(g)
