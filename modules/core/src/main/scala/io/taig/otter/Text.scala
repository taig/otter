package io.taig.otter

import io.taig.otter.operation.*
import io.taig.otter as Self

sealed abstract class Text[+S[a], A] extends Product with Serializable

object Text:
  sealed trait Primitive[A] extends Product with Serializable

  object Primitive:
    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]]) extends Text.Primitive[A]

    object Boolean:
      val liftK = [A] => (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self)
      val unliftK = [A] => (schema: Text.Primitive.Boolean[A]) => schema.self

      given invariant: Invariant[Text.Primitive.Boolean] =
        Invariant[[a] =>> Annotation[Self.Primitive.Boolean[a]]].imapK(liftK)(unliftK)

      // given operation: BooleanOperation[Text.Primitive.Boolean] =
      //   BooleanOperation[[a] =>> Annotation[Self.Primitive.Boolean[a]]].mapK(liftK)

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]]) extends Text.Primitive[A]

    object Number:
      val liftK = [A] => (self: Annotation[Self.Primitive.Number[A]]) => Number(self)
      val unliftK = [A] => (schema: Text.Primitive.Number[A]) => schema.self

      given invariant: Invariant[Text.Primitive.Number] =
        Invariant[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK(liftK)(unliftK)

      // given operation: NumberOperation[Text.Primitive.Number] =
      //   NumberOperation[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK(liftK)(unliftK)

    final case class String[A](self: Annotation[Self.Primitive.String[A]]) extends Text[Nothing, A], Text.Primitive[A]

    object String:
      val liftK = [A] => (self: Annotation[Self.Primitive.String[A]]) => String(self)
      val unliftK = [A] => (schema: Text.Primitive.String[A]) => schema.self

      given invariant: Invariant[Text.Primitive.String] =
        Invariant[[a] =>> Annotation[Self.Primitive.String[a]]].imapK(liftK)(unliftK)

      // given operation: StringOperation[Text.Primitive.String] =
      //   StringOperation[[a] =>> Annotation[Self.Primitive.String[a]]].imapK(liftK)(unliftK)

  final case class Coerce[+S[a] <: Text.Primitive[a], A](self: Annotation[Self.Coerce[S, A]]) extends Text[S, A]

  object Coerce:
    def liftK[S[a] <: Text.Primitive[a]] = [A] => (self: Annotation[Self.Coerce[S, A]]) => Coerce(self)
    def unliftK[S[a] <: Text.Primitive[a]] = [A] => (schema: Text.Coerce[S, A]) => schema.self

    given invariant[S[a] <: Text.Primitive[a]]: Invariant[Text.Coerce[S, *]] =
      Invariant[[a] =>> Annotation[Self.Coerce[S, a]]].imapK(liftK[S])(unliftK[S])

    given operation: CoerceOperation[Text.Primitive, Text.Coerce] = ???
    // CoerceOperation[[a] =>> Annotation[Self.Coerce[Text.Primitive, a]], Text.Primitive].mapK(liftK)

    // given op2: CoerceOperation2[Text.Coerce] = ???

  final case class Constant[+S[a] <: Text[?, a], A](self: Annotation[Self.Constant[S, A]]) extends Text[S, A]

  object Constant:
    def liftK[S[a] <: Text[?, a]] = [A] => (self: Annotation[Self.Constant[S, A]]) => Constant(self)
    def unliftK[S[a] <: Text[?, a]] = [A] => (schema: Text.Constant[S, A]) => schema.self

    given invariant[S[a] <: Text[?, a]]: Invariant[Text.Constant[S, *]] =
      Invariant[[a] =>> Annotation[Self.Constant[S, a]]].imapK(liftK[S])(unliftK[S])

    // given operation[S[a] <: Text[?, a]]: ConstantOperation[Text.Constant[S, *], S] =
    //   ConstantOperation[[a] =>> Annotation[Self.Constant[S, a]], S].mapK(liftK[S])

  final case class Enumeration[+S[a] <: Text[?, a], A](self: Annotation[Self.Enumeration[S, A]]) extends Text[S, A]

  object Enumeration:
    def liftK[S[a] <: Text[?, a]] = [A] => (self: Annotation[Self.Enumeration[S, A]]) => Enumeration(self)
    def unliftK[S[a] <: Text[?, a]] = [A] => (schema: Text.Enumeration[S, A]) => schema.self

    given invariant[S[a] <: Text[?, a]]: Invariant[Text.Enumeration[S, *]] =
      Invariant[[a] =>> Annotation[Self.Enumeration[S, a]]].imapK(liftK[S])(unliftK[S])

    // given operation[S[a] <: Text[?, a]]: EnumerationOperation[Text.Enumeration[S, *], S] =
    //   EnumerationOperation[[a] =>> Annotation[Self.Enumeration[S, a]], S].mapK(liftK[S])

  final case class Union[+S[a] <: Text[?, a], A](self: Annotation[Self.Union[S, A]]) extends Text[S, A]

  object Union:
    def liftK[S[a] <: Text[?, a]] = [A] => (self: Annotation[Self.Union[S, A]]) => Union(self)
    def unliftK[S[a] <: Text[?, a]] = [A] => (schema: Text.Union[S, A]) => schema.self

    given invariant[S[a] <: Text[?, a]]: Invariant[Text.Union[S, *]] =
      Invariant[[a] =>> Annotation[Self.Union[S, a]]].imapK(liftK[S])(unliftK[S])

    // given operation[S[a] <: Text[?, a]]: UnionOperation[Text.Union[S, *], S] =
    //   UnionOperation[[a] =>> Annotation[Self.Union[S, a]], S].mapK(liftK[S])

  given invariant[S[a] <: Text[?, a]]: Invariant[Text[S, *]] with
    extension [A](self: Text[S, A])
      override def imap[B](f: A => B)(g: B => A): Text[S, B] = self match
        case self: Text.Coerce[?, ?]        => self.imap(f)(g)
        case self: Text.Constant[?, ?]      => self.imap(f)(g)
        case self: Text.Enumeration[?, ?]   => self.imap(f)(g)
        case self: Text.Primitive.String[?] => self.imap(f)(g)
        case self: Text.Union[?, ?]         => self.imap(f)(g)
