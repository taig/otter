package io.taig.otter

import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.operation.BooleanOperation
import io.taig.otter.operation.CollectionOperation
import io.taig.otter.operation.ConstantOperation
import io.taig.otter.operation.DictionaryOperation
import io.taig.otter.operation.EnumerationOperation
import io.taig.otter.operation.FieldOperation
import io.taig.otter.operation.NumberOperation
import io.taig.otter.operation.RecordOperation
import io.taig.otter.operation.StringOperation
import io.taig.otter.operation.TupleOperation
import io.taig.otter.operation.UnionOperation

sealed abstract class Schema[+S[a] <: Schema[?, a], A] extends Product with Serializable

object Schema:
  final case class Collection[+S[a] <: Schema[?, a], A](self: Annotation[Self.Collection[S, A]]) extends Schema[S, A]

  object Collection:
    def liftK[S[a] <: Schema[?, a]] = [A] => (self: Annotation[Self.Collection[S, A]]) => Collection(self)
    def unliftK[S[a] <: Schema[?, a]] = [A] => (schema: Schema.Collection[S, A]) => schema.self

    given invariant[S[a] <: Schema[?, a]]: Invariant[Schema.Collection[S, *]] =
      Invariant[[a] =>> Annotation[Self.Collection[S, a]]].imapK(liftK[S])(unliftK[S])

    given operation[S[a] <: Schema[?, a]]: CollectionOperation[Schema.Collection[S, *], S] =
      CollectionOperation[[a] =>> Annotation[Self.Collection[S, a]], S].mapK(liftK[S])

  final case class Constant[+S[a] <: Schema[?, a], A](self: Annotation[Self.Constant[S, A]]) extends Schema[S, A]

  object Constant:
    def liftK[S[a] <: Schema[?, a]] = [A] => (self: Annotation[Self.Constant[S, A]]) => Constant(self)
    def unliftK[S[a] <: Schema[?, a]] = [A] => (schema: Schema.Constant[S, A]) => schema.self

    given invariant[S[a] <: Schema[?, a]]: Invariant[Schema.Constant[S, *]] =
      Invariant[[a] =>> Annotation[Self.Constant[S, a]]].imapK(liftK[S])(unliftK[S])

    given operation[S[a] <: Schema[?, a]]: ConstantOperation[Schema.Constant[S, *], S] =
      ConstantOperation[[a] =>> Annotation[Self.Constant[S, a]], S].mapK(liftK[S])

  final case class Dictionary[+S[a] <: Schema[?, a], A](self: Annotation[Self.Dictionary[S, A]]) extends Schema[S, A]

  object Dictionary:
    def liftK[S[a] <: Schema[?, a]] = [A] => (self: Annotation[Self.Dictionary[S, A]]) => Dictionary(self)
    def unliftK[S[a] <: Schema[?, a]] = [A] => (schema: Schema.Dictionary[S, A]) => schema.self

    given invariant[S[a] <: Schema[?, a]]: Invariant[Schema.Dictionary[S, *]] =
      Invariant[[a] =>> Annotation[Self.Dictionary[S, a]]].imapK(liftK[S])(unliftK[S])

    given operation[S[a] <: Schema[?, a]]: DictionaryOperation[Schema.Dictionary[S, *], S] =
      DictionaryOperation[[a] =>> Annotation[Self.Dictionary[S, a]], S].mapK(liftK[S])

  final case class Enumeration[+S[a] <: Schema[?, a], A](self: Annotation[Self.Enumeration[S, A]]) extends Schema[S, A]

  object Enumeration:
    def liftK[S[a] <: Schema[?, a]] = [A] => (self: Annotation[Self.Enumeration[S, A]]) => Enumeration(self)
    def unliftK[S[a] <: Schema[?, a]] = [A] => (schema: Schema.Enumeration[S, A]) => schema.self

    given invariant[S[a] <: Schema[?, a]]: Invariant[Schema.Enumeration[S, *]] =
      Invariant[[a] =>> Annotation[Self.Enumeration[S, a]]].imapK(liftK[S])(unliftK[S])

    given operation[S[a] <: Schema[?, a]]: EnumerationOperation[Schema.Enumeration[S, *], S] =
      EnumerationOperation[[a] =>> Annotation[Self.Enumeration[S, a]], S].mapK(liftK[S])

  final case class Record[+S[a] <: Schema[?, a], A](self: Annotation[Self.Record[Schema.Field[S, *], A]])
      extends Schema[S, A]

  object Record:
    def liftK[S[a] <: Schema[?, a]] = [A] => (self: Annotation[Self.Record[Schema.Field[S, *], A]]) => Record(self)
    def unliftK[S[a] <: Schema[?, a]] = [A] => (schema: Schema.Record[S, A]) => schema.self

    given invariant[S[a] <: Schema[?, a]]: Invariant[Schema.Record[S, *]] =
      Invariant[[a] =>> Annotation[Self.Record[Schema.Field[S, *], a]]].imapK(liftK[S])(unliftK[S])

    given operation[S[a] <: Schema[?, a]]: RecordOperation[Schema.Record[S, *], Schema.Field[S, *]] =
      RecordOperation[[a] =>> Annotation[Self.Record[Schema.Field[S, *], a]], Schema.Field[S, *]]
        .imapK(liftK[S])(unliftK[S])

  final case class Tuple[+S[a] <: Schema[?, a], A](self: Annotation[Self.Tuple[S, A]]) extends Schema[S, A]

  object Tuple:
    def liftK[S[a] <: Schema[?, a]] = [A] => (self: Annotation[Self.Tuple[S, A]]) => Tuple(self)
    def unliftK[S[a] <: Schema[?, a]] = [A] => (schema: Schema.Tuple[S, A]) => schema.self

    given invariant[S[a] <: Schema[?, a]]: Invariant[Schema.Tuple[S, *]] =
      Invariant[[a] =>> Annotation[Self.Tuple[S, a]]].imapK(liftK[S])(unliftK[S])

    given operation[S[a] <: Schema[?, a]]: TupleOperation[Schema.Tuple[S, *], S] =
      TupleOperation[[a] =>> Annotation[Self.Tuple[S, a]], S].imapK(liftK[S])(unliftK[S])

  final case class Union[+S[a] <: Schema[?, a], A](self: Annotation[Self.Union[S, A]]) extends Schema[S, A]

  object Union:
    def liftK[S[a] <: Schema[?, a]] = [A] => (self: Annotation[Self.Union[S, A]]) => Union(self)
    def unliftK[S[a] <: Schema[?, a]] = [A] => (schema: Schema.Union[S, A]) => schema.self

    given invariant[S[a] <: Schema[?, a]]: Invariant[Schema.Union[S, *]] =
      Invariant[[a] =>> Annotation[Self.Union[S, a]]].imapK(liftK[S])(unliftK[S])

    given operation[S[a] <: Schema[?, a]]: UnionOperation[Schema.Union[S, *], S] =
      UnionOperation[[a] =>> Annotation[Self.Union[S, a]], S].imapK(liftK[S])(unliftK[S])

  final case class Field[+S[a] <: Schema[?, a], A](self: Annotation[Self.Field[S, A]]) extends AnyVal

  object Field:
    def liftK[S[a] <: Schema[?, a]] = [A] => (self: Annotation[Self.Field[S, A]]) => Field(self)
    def unliftK[S[a] <: Schema[?, a]] = [A] => (schema: Schema.Field[S, A]) => schema.self

    given invariant[S[a] <: Schema[?, a]]: Invariant[Schema.Field[S, *]] =
      Invariant[[a] =>> Annotation[Self.Field[S, a]]].imapK(liftK[S])(unliftK[S])

    given operation[S[a] <: Schema[?, a]]: FieldOperation[Schema.Field[S, *], S] =
      FieldOperation[[a] =>> Annotation[Self.Field[S, a]], S].imapK(liftK[S])(unliftK[S])

  sealed abstract class Primitive[A] extends Schema[Nothing, A]

  object Primitive:
    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]]) extends Schema.Primitive[A]

    object Boolean:
      def liftK = [A] => (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self)
      def unliftK = [A] => (schema: Schema.Primitive.Boolean[A]) => schema.self

      given invariant: Invariant[Schema.Primitive.Boolean] =
        Invariant[[a] =>> Annotation[Self.Primitive.Boolean[a]]].imapK(liftK)(unliftK)

      given operation: BooleanOperation[Schema.Primitive.Boolean] =
        BooleanOperation[[a] =>> Annotation[Self.Primitive.Boolean[a]]].mapK(liftK)

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]]) extends Schema.Primitive[A]

    object Number:
      def liftK = [A] => (self: Annotation[Self.Primitive.Number[A]]) => Number(self)
      def unliftK = [A] => (schema: Schema.Primitive.Number[A]) => schema.self

      given invariant: Invariant[Schema.Primitive.Number] =
        Invariant[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK(liftK)(unliftK)

      given operation: NumberOperation[Schema.Primitive.Number] =
        NumberOperation[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK(liftK)(unliftK)

    final case class String[A](self: Annotation[Self.Primitive.String[A]]) extends Schema.Primitive[A]

    object String:
      def liftK = [A] => (self: Annotation[Self.Primitive.String[A]]) => String(self)
      def unliftK = [A] => (schema: Schema.Primitive.String[A]) => schema.self

      given invariant: Invariant[Schema.Primitive.String] =
        Invariant[[a] =>> Annotation[Self.Primitive.String[a]]].imapK(liftK)(unliftK)

      given operation: StringOperation[Schema.Primitive.String] =
        StringOperation[[a] =>> Annotation[Self.Primitive.String[a]]].imapK(liftK)(unliftK)

    given invariant[A]: Invariant[Schema.Primitive] with
      extension [A](self: Schema.Primitive[A])
        override def imap[B](f: A => B)(g: B => A): Schema.Primitive[B] = self match
          case self: Schema.Primitive.Boolean[A] => self.imap(f)(g)
          case self: Schema.Primitive.Number[A]  => self.imap(f)(g)
          case self: Schema.Primitive.String[A]  => self.imap(f)(g)

  given invariant[S[a] <: Schema[?, a]]: Invariant[Schema[S, *]] with
    extension [A](self: Schema[S, A])
      override def imap[B](f: A => B)(g: B => A): Schema[S, B] = self match
        case self: Schema.Collection[S, A]  => self.imap(f)(g)
        case self: Schema.Constant[S, A]    => self.imap(f)(g)
        case self: Schema.Dictionary[S, A]  => self.imap(f)(g)
        case self: Schema.Enumeration[S, A] => self.imap(f)(g)
        case self: Schema.Record[S, A]      => self.imap(f)(g)
        case self: Schema.Primitive[A]      => self.imap(f)(g)
        case self: Schema.Tuple[S, A]       => self.imap(f)(g)
        case self: Schema.Union[S, A]       => self.imap(f)(g)
