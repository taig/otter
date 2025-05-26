package io.taig.otter.http

import io.taig.otter as Self
import io.taig.otter.Key
import io.taig.otter.operation.*

sealed abstract class FormData[A] extends Product with Serializable

object FormData:
  final case class Dictionary[A](self: Self.Dictionary[Key, FormData.Value, A]) extends FormData[A]

  object Dictionary:
    given DictionarySchemaInvariant[FormData.Dictionary, Key, FormData.Value] =
      DictionarySchemaInvariant[Self.Dictionary[Key, FormData.Value, *], Key, FormData.Value]
        .imapK(
          [A] => (schema: Self.Dictionary[Key, FormData.Value, A]) => Dictionary(schema)
        )([A] => (formData: FormData.Dictionary[A]) => formData.self)

  final case class Record[A](self: Self.Record[FormData.Field, A]) extends FormData[A]

  object Record:
    given RecordSchemaInvariant[FormData.Record, FormData.Field] =
      RecordSchemaInvariant[Self.Record[FormData.Field, *], FormData.Field]
        .imapK(
          [A] => (schema: Self.Record[FormData.Field, A]) => Record(schema)
        )([A] => (formData: FormData.Record[A]) => formData.self)

  sealed abstract class Value[A] extends Product with Serializable

  object Value:
    final case class Constant[A](self: Self.Constant[FormData.Value, A]) extends Value[A]

    object Constant:
      given ConstantSchemaInvariant[FormData.Value.Constant, FormData.Value] =
        ConstantSchemaInvariant[Self.Constant[FormData.Value, *], FormData.Value]
          .imapK(
            [A] => (schema: Self.Constant[FormData.Value, A]) => Constant(schema)
          )([A] => (formData: FormData.Value.Constant[A]) => formData.self)

    final case class Enumeration[A](self: Self.Enumeration[FormData.Value, A]) extends Value[A]

    object Enumeration:
      given EnumerationSchemaInvariant[FormData.Value.Enumeration, FormData.Value] =
        EnumerationSchemaInvariant[Self.Enumeration[FormData.Value, *], FormData.Value]
          .imapK(
            [A] => (schema: Self.Enumeration[FormData.Value, A]) => Enumeration(schema)
          )([A] => (formData: FormData.Value.Enumeration[A]) => formData.self)

    final case class Nullable[A](self: Self.Nullable[FormData.Value, A]) extends Value[A]

    object Nullable:
      given NullableSchemaInvariant[FormData.Value.Nullable, FormData.Value] =
        NullableSchemaInvariant[Self.Nullable[FormData.Value, *], FormData.Value]
          .imapK(
            [A] => (schema: Self.Nullable[FormData.Value, A]) => Nullable(schema)
          )([A] => (formData: FormData.Value.Nullable[A]) => formData.self)

    final case class Primitive[A](self: Self.Primitive.String[A]) extends Value[A]

    object Primitive:
      given PrimitiveSchemaInvariant.String[FormData.Value.Primitive] = PrimitiveSchemaInvariant
        .String[Self.Primitive.String]
        .imapK(
          [A] => (schema: Self.Primitive.String[A]) => Primitive(schema)
        )([A] => (formData: FormData.Value.Primitive[A]) => formData.self)

    final case class Union[A](self: Self.Union[FormData.Value, A]) extends Value[A]

    object Union:
      given UnionSchemaInvariant[FormData.Value.Union, FormData.Value] =
        UnionSchemaInvariant[Self.Union[FormData.Value, *], FormData.Value]
          .imapK(
            [A] => (schema: Self.Union[FormData.Value, A]) => Union(schema)
          )([A] => (formData: FormData.Value.Union[A]) => formData.self)

  final case class Field[A](self: Self.Field[Key, FormData.Value, A])

  object Field:
    given FieldSchemaInvariant[FormData.Field, Key, FormData.Value] =
      FieldSchemaInvariant[Self.Field[Key, FormData.Value, *], Key, FormData.Value]
        .imapK(
          [A] => (schema: Self.Field[Key, FormData.Value, A]) => Field(schema)
        )([A] => (formData: FormData.Field[A]) => formData.self)
