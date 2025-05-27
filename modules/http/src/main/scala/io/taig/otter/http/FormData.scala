package io.taig.otter.http

import io.taig.otter as Self
import io.taig.otter.Key
import io.taig.otter.operation.*

sealed abstract class FormData[A] extends Product with Serializable

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

  sealed abstract class Schema[A] extends Product with Serializable

  object Schema:
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

    final case class Primitive[A](self: Self.Primitive.String[A]) extends Schema[A]

    object Primitive:
      given PrimitiveSchemaInvariant.String[FormData.Schema.Primitive] = PrimitiveSchemaInvariant
        .String[Self.Primitive.String]
        .imapK(
          [A] => (schema: Self.Primitive.String[A]) => Primitive(schema)
        )([A] => (formData: FormData.Schema.Primitive[A]) => formData.self)

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
