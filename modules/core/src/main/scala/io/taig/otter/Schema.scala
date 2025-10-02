package io.taig.otter

import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.operation.BooleanOperation
import io.taig.otter.operation.FieldOperation
import io.taig.otter.operation.NumberOperation
import io.taig.otter.operation.PrimitiveOperation
import io.taig.otter.operation.RecordOperation
import io.taig.otter.operation.StringOperation
import io.taig.validation.Validation
import cats.data.Chain

sealed abstract class Schema[A] extends Product with Serializable

object Schema:
  sealed abstract class Primitive[A] extends Schema[A]:
    def self: Annotation[Self.Primitive[A]]

  object Primitive:
    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]]) extends Schema.Primitive[A]

    object Boolean:
      val liftK = [A] => (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self)
      val unliftK = [A] => (schema: Schema.Primitive.Boolean[A]) => schema.self

      given invariant: Invariant[Schema.Primitive.Boolean] =
        Invariant[[a] =>> Annotation[Self.Primitive.Boolean[a]]].imapK(liftK)(unliftK)

      given operation: BooleanOperation[Schema.Primitive.Boolean] =
        BooleanOperation[[a] =>> Annotation[Self.Primitive.Boolean[a]]].mapK(liftK)

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]]) extends Schema.Primitive[A]

    object Number:
      val liftK = [A] => (self: Annotation[Self.Primitive.Number[A]]) => Number(self)
      val unliftK = [A] => (schema: Schema.Primitive.Number[A]) => schema.self

      given invariant: Invariant[Schema.Primitive.Number] =
        Invariant[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK(liftK)(unliftK)

      given operation: NumberOperation[Schema.Primitive.Number] =
        NumberOperation[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK(liftK)(unliftK)

    final case class String[A](self: Annotation[Self.Primitive.String[A]]) extends Schema.Primitive[A]

    object String:
      val liftK = [A] => (self: Annotation[Self.Primitive.String[A]]) => String(self)
      val unliftK = [A] => (schema: Schema.Primitive.String[A]) => schema.self

      given invariant: Invariant[Schema.Primitive.String] =
        Invariant[[a] =>> Annotation[Self.Primitive.String[a]]].imapK(liftK)(unliftK)

      given operation: StringOperation[Schema.Primitive.String] =
        StringOperation[[a] =>> Annotation[Self.Primitive.String[a]]].imapK(liftK)(unliftK)

    given Invariant[Schema.Primitive] with
      extension [A](self: Primitive[A])
        override def imap[B](f: A => B)(g: B => A): Primitive[B] = self match
          case schema: Schema.Primitive.Boolean[A] => schema.imap(f)(g)
          case schema: Schema.Primitive.Number[A]  => schema.imap(f)(g)
          case schema: Schema.Primitive.String[A]  => schema.imap(f)(g)

    given operation: PrimitiveOperation[Schema.Primitive] with
      export Boolean.operation.boolean
      export Number.operation.{bigDecimal, bigInteger, double, float, int, long}
      export String.operation.{parser, string}

      extension [A](schema: Schema.Primitive[A])
        override def constraints: Chain[Constraint.Primitive.Number | Constraint.Primitive.Text] =
          schema.self.self.constraints

  final case class Record[S[a] <: Schema[a], A](self: Annotation[Self.Record[Schema.Field[S, *], A]]) extends Schema[A]

  object Record:
    def liftK[S[a] <: Schema[a]] = [A] =>
      (self: Annotation[Self.Record[[a] =>> Annotation[Self.Field[S, a]], A]]) => Record(self)
    def unliftK[S[a] <: Schema[a]] = [A] => (schema: Schema.Record[S, A]) => schema.self

    given [S[a] <: Schema[a]]: Invariant[Schema.Record[S, *]] =
      Invariant[[a] =>> Annotation[Self.Record[Schema.Field[S, *], a]]].imapK(liftK[S])(unliftK[S])

    given [S[a] <: Schema[a]]: RecordOperation[Schema.Record[S, *], Schema.Field[S, *]] =
      RecordOperation[
        [a] =>> Annotation[Self.Record[[b] =>> Annotation[Self.Field[S, b]], a]],
        [a] =>> Annotation[Self.Field[S, a]]
      ].imapK(liftK[S])(unliftK[S])

  opaque type Field[S[a] <: Schema[a], A] = Annotation[Self.Field[S, A]]

  object Field:
    extension [S[a] <: Schema[a], A](self: Schema.Field[S, A]) inline def self: Annotation[Self.Field[S, A]] = self

    inline given [S[a] <: Schema[a]](using
        invariant: Invariant[[a] =>> Annotation[Self.Field[S, a]]]
    ): Invariant[Schema.Field[S, *]] = invariant

    inline given [S[a] <: Schema[a]](using
        operation: FieldOperation[[a] =>> Annotation[Self.Field[S, a]], S]
    ): FieldOperation[Schema.Field[S, *], S] = operation

  given Invariant[Schema] with
    extension [A](self: Schema[A])
      override def imap[B](f: A => B)(g: B => A): Schema[B] = self match
        case schema: Schema.Record[?, A] => schema.imap(f)(g)
        case schema: Schema.Primitive[A] => schema.imap(f)(g)
