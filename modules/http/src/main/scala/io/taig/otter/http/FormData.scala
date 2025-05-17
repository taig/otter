package io.taig.otter.http

import io.taig.otter as Self
import Self.http.FormData.Dictionary
import Self.schema.DictionarySchema
import Self.schema.RecordSchema
import Self.schema.FieldSchema
import Self.schema.PrimitiveSchema
import Self.schema.NullableSchema

sealed abstract class FormData[A] extends Product with Serializable

object FormData:
  final case class Dictionary[A](self: Self.Dictionary[FormData.Key, FormData.Value, A]) extends FormData[A]

  object Dictionary:
    given DictionarySchema[FormData.Dictionary, FormData.Key, FormData.Value] =
      DictionarySchema[Self.Dictionary[FormData.Key, FormData.Value, *], FormData.Key, FormData.Value]
        .imapK(
          [A] => (schema: Self.Dictionary[FormData.Key, FormData.Value, A]) => Dictionary(schema)
        )([A] => (formData: FormData.Dictionary[A]) => formData.self)

  final case class Record[A](self: Self.Record[FormData.Field, A]) extends FormData[A]

  object Record:
    given RecordSchema[FormData.Record, FormData.Field] =
      RecordSchema[Self.Record[FormData.Field, *], FormData.Field]
        .imapK(
          [A] => (schema: Self.Record[FormData.Field, A]) => Record(schema)
        )([A] => (formData: FormData.Record[A]) => formData.self)

  sealed abstract class Key[A] extends Product with Serializable

  object Key:
    final case class Primitive[A](self: Self.Primitive.String[A]) extends FormData.Key[A]

    object Primitive:
      given PrimitiveSchema.String[FormData.Key.Primitive] = PrimitiveSchema
        .String[Self.Primitive.String]
        .imapK(
          [A] => (schema: Self.Primitive.String[A]) => Primitive(schema)
        )([A] => (formData: FormData.Key.Primitive[A]) => formData.self)

  sealed abstract class Value[A] extends Product with Serializable

  object Value:
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

  final case class Field[A](self: Self.Field[FormData.Key, FormData.Value, A])

  object Field:
    given FieldSchema[FormData.Field, FormData.Key, FormData.Value] =
      FieldSchema[Self.Field[FormData.Key, FormData.Value, *], FormData.Key, FormData.Value]
        .imapK(
          [A] => (schema: Self.Field[FormData.Key, FormData.Value, A]) => Field(schema)
        )([A] => (formData: FormData.Field[A]) => formData.self)
