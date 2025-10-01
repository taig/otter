package io.taig.otter

import io.taig.otter as Self
import cats.syntax.all.*
import io.taig.otter.operation.FieldOperation
import io.taig.otter.operation.BooleanOperation
import Self.operation.RecordOperation
import Self.operation.NumberOperation
import Self.operation.StringOperation
import Self.operation.PrimitiveOperation

sealed abstract class Schema[A] extends Product with Serializable

object Schema:
  sealed abstract class Primitive[A] extends Schema[A]:
    def self: Annotation[Self.Primitive[A]]

  object Primitive:
    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]]) extends Schema.Primitive[A]

    object Boolean:
      val liftK = [A] => (self: Annotation[Self.Primitive.Boolean[A]]) => Boolean(self)
      val unliftK = [A] => (schema: Schema.Primitive.Boolean[A]) => schema.self

      given Invariant[Schema.Primitive.Boolean] =
        Invariant[[a] =>> Annotation[Self.Primitive.Boolean[a]]].imapK(liftK)(unliftK)

      given BooleanOperation[Schema.Primitive.Boolean] =
        BooleanOperation[[a] =>> Annotation[Self.Primitive.Boolean[a]]].mapK(liftK)

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]]) extends Schema.Primitive[A]

    object Number:
      val liftK = [A] => (self: Annotation[Self.Primitive.Number[A]]) => Number(self)
      val unliftK = [A] => (schema: Schema.Primitive.Number[A]) => schema.self

      given Invariant[Schema.Primitive.Number] =
        Invariant[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK(liftK)(unliftK)

      given NumberOperation[Schema.Primitive.Number] =
        NumberOperation[[a] =>> Annotation[Self.Primitive.Number[a]]].imapK(liftK)(unliftK)

    final case class String[A](self: Annotation[Self.Primitive.String[A]]) extends Schema.Primitive[A]

    object String:
      val liftK = [A] => (self: Annotation[Self.Primitive.String[A]]) => String(self)
      val unliftK = [A] => (schema: Schema.Primitive.String[A]) => schema.self

      given Invariant[Schema.Primitive.String] =
        Invariant[[a] =>> Annotation[Self.Primitive.String[a]]].imapK(liftK)(unliftK)

      given StringOperation[Schema.Primitive.String] =
        StringOperation[[a] =>> Annotation[Self.Primitive.String[a]]].imapK(liftK)(unliftK)

    def apply[A](self: Annotation[Self.Primitive[A]]): Schema.Primitive[A] = self match
      case Annotation(_, _: Self.Primitive.Boolean[A]) =>
        Boolean(self.asInstanceOf[Annotation[Self.Primitive.Boolean[A]]])
      case Annotation(_, _: Self.Primitive.Number[A]) =>
        Number(self.asInstanceOf[Annotation[Self.Primitive.Number[A]]])
      case Annotation(_, _: Self.Primitive.String[A]) =>
        String(self.asInstanceOf[Annotation[Self.Primitive.String[A]]])

    val liftK = [A] => (self: Annotation[Self.Primitive[A]]) => Primitive(self)
    val unliftK = [A] => (schema: Schema.Primitive[A]) => schema.self

    given Invariant[Schema.Primitive] = Invariant[[a] =>> Annotation[Self.Primitive[a]]].imapK(liftK)(unliftK)

    given operation: PrimitiveOperation[Schema.Primitive] =
      PrimitiveOperation[[a] =>> Annotation[Self.Primitive[a]]].imapK(liftK)(unliftK)

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
