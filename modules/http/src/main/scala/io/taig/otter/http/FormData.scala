package io.taig.otter.http

import io.taig.otter as Self
import io.taig.otter.Key
import io.taig.otter.operation.*

sealed abstract class FormData[A] extends Product, Serializable

object FormData:
  final case class Dictionary[A](self: Self.Dictionary[Key, FormData.Schema, A]) extends FormData[A]

  object Dictionary:
    given DictionarySchemaInvariant[FormData.Dictionary, Key, FormData.Schema] =
      DictionarySchemaInvariant[Self.Dictionary[Key, FormData.Schema, *], Key, FormData.Schema]
        .imapK(
          [A] => (schema: Self.Dictionary[Key, FormData.Schema, A]) => Dictionary(schema)
        )([A] => (formData: FormData.Dictionary[A]) => formData.self)

  final case class Record[A](self: Self.Record[FormData.Field, A]) extends FormData[A]

  object Record:
    given RecordSchemaInvariant[FormData.Record, FormData.Field] =
      RecordSchemaInvariant[Self.Record[FormData.Field, *], FormData.Field]
        .imapK(
          [A] => (schema: Self.Record[FormData.Field, A]) => Record(schema)
        )([A] => (formData: FormData.Record[A]) => formData.self)

  sealed trait Schema[A] extends FormData.Schema.Any[A]

  object Schema:
    sealed trait Any[A] extends Product, Serializable

    sealed trait Primitive[A] extends FormData.Schema.Any[A]:
      def self: Self.Primitive[FormData.Schema.Primitive, A]

    object Primitive:
      final case class Boolean[A](self: Self.Primitive.Boolean[A]) extends FormData.Schema.Primitive[A]

      object Boolean:
        given PrimitiveSchemaInvariant.Boolean[FormData.Schema.Primitive.Boolean] =
          PrimitiveSchemaInvariant
            .Boolean[Self.Primitive.Boolean]
            .imapK(
              [A] => (schema: Self.Primitive.Boolean[A]) => Boolean(schema)
            )([A] => (formData: FormData.Schema.Primitive.Boolean[A]) => formData.self)

      final case class Number[A](self: Self.Primitive.Number[A]) extends FormData.Schema.Primitive[A]

      object Number:
        given PrimitiveSchemaInvariant.Number[FormData.Schema.Primitive.Number] =
          PrimitiveSchemaInvariant
            .Number[Self.Primitive.Number]
            .imapK(
              [A] => (schema: Self.Primitive.Number[A]) => Number(schema)
            )([A] => (formData: FormData.Schema.Primitive.Number[A]) => formData.self)

      final case class String[A](self: Self.Primitive.String[FormData.Schema.Primitive, A])
          extends FormData.Schema.Primitive[A],
            FormData.Schema[A]

      object String:
        given PrimitiveSchemaInvariant.String[FormData.Schema.Primitive.String, FormData.Schema.Primitive] =
          PrimitiveSchemaInvariant
            .String[Self.Primitive.String[FormData.Schema.Primitive, *], FormData.Schema.Primitive]
            .imapK(
              [A] => (schema: Self.Primitive.String[FormData.Schema.Primitive, A]) => String(schema)
            )([A] => (formData: FormData.Schema.Primitive.String[A]) => formData.self)

      given PrimitiveSchemaInvariant[FormData.Schema.Primitive, FormData.Schema.Primitive] =
        PrimitiveSchemaInvariant[Self.Primitive[FormData.Schema.Primitive, *], FormData.Schema.Primitive]
          .imapK(
            [A] =>
              (schema: Self.Primitive[FormData.Schema.Primitive, A]) =>
                schema match
                  case self: Self.Primitive.Boolean[A]                           => Boolean(self)
                  case self: Self.Primitive.Number[A]                            => Number(self)
                  case self: Self.Primitive.String[FormData.Schema.Primitive, A] => String(self)
          )([A] => (formData: FormData.Schema.Primitive[A]) => formData.self)

      given Parseable[FormData.Schema.Primitive, FormData.Schema.Primitive.String] =
        Parseable[FormData.Schema.Primitive, FormData.Schema.Primitive.String]

    final case class Constant[A](self: Self.Constant[FormData.Schema, A]) extends Schema[A]

    object Constant:
      given ConstantSchemaInvariant[FormData.Schema.Constant, FormData.Schema] =
        ConstantSchemaInvariant[Self.Constant[FormData.Schema, *], FormData.Schema]
          .imapK(
            [A] => (schema: Self.Constant[FormData.Schema, A]) => Constant(schema)
          )([A] => (formData: FormData.Schema.Constant[A]) => formData.self)

    final case class Enumeration[A](self: Self.Enumeration[FormData.Schema, A]) extends Schema[A]

    object Enumeration:
      given EnumerationSchemaInvariant[FormData.Schema.Enumeration, FormData.Schema] =
        EnumerationSchemaInvariant[Self.Enumeration[FormData.Schema, *], FormData.Schema]
          .imapK(
            [A] => (schema: Self.Enumeration[FormData.Schema, A]) => Enumeration(schema)
          )([A] => (formData: FormData.Schema.Enumeration[A]) => formData.self)

    final case class Nullable[A](self: Self.Nullable[FormData.Schema, A]) extends Schema[A]

    object Nullable:
      given NullableSchemaInvariant[FormData.Schema.Nullable, FormData.Schema] =
        NullableSchemaInvariant[Self.Nullable[FormData.Schema, *], FormData.Schema]
          .imapK(
            [A] => (schema: Self.Nullable[FormData.Schema, A]) => Nullable(schema)
          )([A] => (formData: FormData.Schema.Nullable[A]) => formData.self)

    final case class Union[A](self: Self.Union[FormData.Schema, A]) extends Schema[A]

    object Union:
      given UnionSchemaInvariant[FormData.Schema.Union, FormData.Schema] =
        UnionSchemaInvariant[Self.Union[FormData.Schema, *], FormData.Schema]
          .imapK(
            [A] => (schema: Self.Union[FormData.Schema, A]) => Union(schema)
          )([A] => (formData: FormData.Schema.Union[A]) => formData.self)

  final case class Field[A](self: Self.Field[Key, FormData.Schema, A])

  object Field:
    given FieldSchemaInvariant[FormData.Field, Key, FormData.Schema] =
      FieldSchemaInvariant[Self.Field[Key, FormData.Schema, *], Key, FormData.Schema]
        .imapK(
          [A] => (schema: Self.Field[Key, FormData.Schema, A]) => Field(schema)
        )([A] => (formData: FormData.Field[A]) => formData.self)
