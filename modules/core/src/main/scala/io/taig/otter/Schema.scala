package io.taig.otter

import io.taig.otter as Self
import cats.syntax.all.*
import io.taig.otter.operation.BooleanSchemaInvariant
import Self.operation.NumberSchemaInvariant
import Self.operation.StringSchemaInvariant
import Self.operation.PrimitiveSchemaInvariant
import Self.operation.RecordSchemaInvariant
import Self.operation.FieldSchemaInvariant
import Self.operation.SchemaInvariant

sealed abstract class Schema[A] extends Product with Serializable

object Schema:
  sealed abstract class Primitive[A] extends Schema[A]

  object Primitive:
    final case class Boolean[A](self: Annotation[Self.Primitive.Boolean[A]]) extends Schema.Primitive[A]

    object Boolean:
      given BooleanSchemaInvariant[Schema.Primitive.Boolean] =
        val fK = [A] => (self: Annotation[Self.Primitive.Boolean[A]]) => Schema.Primitive.Boolean(self)
        val gK = [A] => (schema: Schema.Primitive.Boolean[A]) => schema.self
        BooleanSchemaInvariant[[a] =>> Annotation[Self.Primitive.Boolean[a]]].imapK(fK)(gK)

    final case class Number[A](self: Annotation[Self.Primitive.Number[A]]) extends Schema.Primitive[A]

    object Number:
      given NumberSchemaInvariant[Schema.Primitive.Number, Constraint.Primitive.Number] =
        val fK = [A] => (self: Annotation[Self.Primitive.Number[A]]) => Schema.Primitive.Number(self)
        val gK = [A] => (schema: Schema.Primitive.Number[A]) => schema.self
        NumberSchemaInvariant[[a] =>> Annotation[Self.Primitive.Number[a]], Constraint.Primitive.Number].imapK(fK)(gK)

    final case class String[A](self: Annotation[Self.Primitive.String[A]]) extends Schema.Primitive[A]

    object String:
      given StringSchemaInvariant[Schema.Primitive.String, Constraint.Primitive.Text] =
        val fK = [A] => (self: Annotation[Self.Primitive.String[A]]) => Schema.Primitive.String(self)
        val gK = [A] => (schema: Schema.Primitive.String[A]) => schema.self
        StringSchemaInvariant[[a] =>> Annotation[Self.Primitive.String[a]], Constraint.Primitive.Text].imapK(fK)(gK)

    given PrimitiveSchemaInvariant[Schema.Primitive] =
      val fK = [A] =>
        (self: Annotation[Self.Primitive[A]]) =>
          self match
            case Annotation(metadata, self: Self.Primitive.Boolean[A]) =>
              Schema.Primitive.Boolean(Annotation(metadata, self))
            case Annotation(metadata, self: Self.Primitive.Number[A]) =>
              Schema.Primitive.Number(Annotation(metadata, self))
            case Annotation(metadata, self: Self.Primitive.String[A]) =>
              Schema.Primitive.String(Annotation(metadata, self))
      val gK = [A] =>
        (schema: Schema.Primitive[A]) =>
          schema match
            case schema: Schema.Primitive.Boolean[A] => schema.self
            case schema: Schema.Primitive.Number[A]  => schema.self
            case schema: Schema.Primitive.String[A]  => schema.self
      PrimitiveSchemaInvariant[[a] =>> Annotation[Self.Primitive[a]]].imapK(fK)(gK)

  final case class Record[S[a] <: Schema[a], A](self: Annotation[Self.Record[Schema.Field[S, *], A]]) extends Schema[A]

  object Record:
    given [S[a] <: Schema[a]]: RecordSchemaInvariant[Schema.Record[S, *], Schema.Field[S, *]] =
      val fK = [A] => (self: Annotation[Self.Record[[a] =>> Annotation[Self.Field[S, a]], A]]) => Schema.Record(self)
      val gK = [A] => (schema: Schema.Record[S, A]) => schema.self
      RecordSchemaInvariant[
        [a] =>> Annotation[Self.Record[[a] =>> Annotation[Self.Field[S, a]], a]],
        [a] =>> Annotation[Self.Field[S, a]]
      ].imapK(fK)(gK)

  opaque type Field[S[a] <: Schema[a], A] = Annotation[Self.Field[S, A]]

  object Field:
    given [S[a] <: Schema[a]]: FieldSchemaInvariant[Schema.Field[S, *], S] = FieldSchemaInvariant.schema[S]

  given SchemaInvariant[Schema] with
    extension [A](self: Schema[A])
      override def imap[B](f: A => B)(g: B => A): Schema[B] = self match
        case Schema.Primitive.Boolean(self) => Schema.Primitive.Boolean(self.map(_.imap(f)(g)))
        case Schema.Primitive.Number(self)  => Schema.Primitive.Number(self.map(_.imap(f)(g)))
        case Schema.Primitive.String(self)  => Schema.Primitive.String(self.map(_.imap(f)(g)))
        case Schema.Record(self)            => Schema.Record(self.map(_.imap(f)(g)))
