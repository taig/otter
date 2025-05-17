package io.taig.otter.http

import io.taig.otter as Self
import io.taig.otter.http.FormData.Dictionary
import io.taig.otter.schema.DictionarySchema
import io.taig.otter.schema.RecordSchema
import io.taig.otter.schema.FieldSchema
import io.taig.otter.schema.PrimitiveSchema
import io.taig.otter.schema.NullableSchema
import Self.Key
import Self.schema.ConstantSchema
import Self.schema.EnumerationSchema
import Self.schema.UnionSchema

sealed abstract class FormData[A] extends Product with Serializable

object FormData:
  final case class Dictionary[A](self: Self.Dictionary[Key, FormData.Value, A]) extends FormData[A]

  object Dictionary:
    given DictionarySchema[FormData.Dictionary, Key, FormData.Value] =
      DictionarySchema[Self.Dictionary[Key, FormData.Value, *], Key, FormData.Value]
        .imapK(
          [A] => (schema: Self.Dictionary[Key, FormData.Value, A]) => Dictionary(schema)
        )([A] => (formData: FormData.Dictionary[A]) => formData.self)

  final case class Record[A](self: Self.Record[FormData.Field, A]) extends FormData[A]

  object Record:
    given RecordSchema[FormData.Record, FormData.Field] =
      RecordSchema[Self.Record[FormData.Field, *], FormData.Field]
        .imapK(
          [A] => (schema: Self.Record[FormData.Field, A]) => Record(schema)
        )([A] => (formData: FormData.Record[A]) => formData.self)

  sealed abstract class Value[A] extends Product with Serializable

  object Value:
    final case class Constant[A](self: Self.Constant[FormData.Value, A]) extends Value[A]

    object Constant:
      given ConstantSchema[FormData.Value.Constant, FormData.Value] =
        ConstantSchema[Self.Constant[FormData.Value, *], FormData.Value]
          .imapK(
            [A] => (schema: Self.Constant[FormData.Value, A]) => Constant(schema)
          )([A] => (formData: FormData.Value.Constant[A]) => formData.self)

    final case class Enumeration[A](self: Self.Enumeration[FormData.Value, A]) extends Value[A]

    object Enumeration:
      given EnumerationSchema[FormData.Value.Enumeration, FormData.Value] =
        EnumerationSchema[Self.Enumeration[FormData.Value, *], FormData.Value]
          .imapK(
            [A] => (schema: Self.Enumeration[FormData.Value, A]) => Enumeration(schema)
          )([A] => (formData: FormData.Value.Enumeration[A]) => formData.self)

    final case class Nullable[A](self: Self.Nullable[FormData.Value, A]) extends Value[A]

    object Nullable:
      given NullableSchema[FormData.Value.Nullable, FormData.Value] =
        NullableSchema[Self.Nullable[FormData.Value, *], FormData.Value]
          .imapK(
            [A] => (schema: Self.Nullable[FormData.Value, A]) => Nullable(schema)
          )([A] => (formData: FormData.Value.Nullable[A]) => formData.self)

    final case class Primitive[A](self: Self.Primitive.String[A]) extends Value[A]

    object Primitive:
      given PrimitiveSchema.String[FormData.Value.Primitive] = PrimitiveSchema
        .String[Self.Primitive.String]
        .imapK(
          [A] => (schema: Self.Primitive.String[A]) => Primitive(schema)
        )([A] => (formData: FormData.Value.Primitive[A]) => formData.self)

    final case class Union[A](self: Self.Union[FormData.Value, A]) extends Value[A]

    object Union:
      given UnionSchema[FormData.Value.Union, FormData.Value] =
        UnionSchema[Self.Union[FormData.Value, *], FormData.Value]
          .imapK(
            [A] => (schema: Self.Union[FormData.Value, A]) => Union(schema)
          )([A] => (formData: FormData.Value.Union[A]) => formData.self)

  final case class Field[A](self: Self.Field[Key, FormData.Value, A])

  object Field:
    given FieldSchema[FormData.Field, Key, FormData.Value] =
      FieldSchema[Self.Field[Key, FormData.Value, *], Key, FormData.Value]
        .imapK(
          [A] => (schema: Self.Field[Key, FormData.Value, A]) => Field(schema)
        )([A] => (formData: FormData.Field[A]) => formData.self)
