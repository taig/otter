package io.taig.otter.http

import io.taig.otter as Self
import io.taig.otter.Codec
import Self.http.FormData.Dictionary

sealed abstract class FormData[A] extends Product with Serializable

object FormData:
  final case class Dictionary[A](self: Self.Dictionary[FormData.Key, FormData.Value, A]) extends FormData[A]

  object Dictionary:
    given codec: Codec.Dictionary[FormData.Dictionary, FormData.Key, FormData.Value] = Codec.Dictionary(
      lift = [A] => (self: Self.Dictionary[FormData.Key, FormData.Value, A]) => Dictionary(self),
      extract = [A] => (codec: FormData.Dictionary[A]) => codec.self
    )

  final case class Record[A](self: Self.Record[FormData.Field, A]) extends FormData[A]

  object Record:
    given codec: Codec.Record[FormData.Record, FormData.Field] =
      Codec.Record(
        lift = [A] => (self: Self.Record[FormData.Field, A]) => Record(self),
        extract = [A] => (codec: FormData.Record[A]) => codec.self
      )

  sealed abstract class Key[A] extends Product with Serializable

  object Key:
    final case class Primitive[A](self: Self.Primitive.String[A]) extends FormData.Key[A]

    object Primitive:
      given codec: Codec.Primitive.String[FormData.Key.Primitive] = Codec.Primitive.String(
        lift = [A] => (self: Self.Primitive.String[A]) => Primitive(self),
        extract = [A] => (codec: FormData.Key.Primitive[A]) => codec.self
      )

  sealed abstract class Value[A] extends Product with Serializable

  object Value:
    final case class Nullable[A](self: Self.Nullable[FormData.Value, A]) extends Value[A]

    object Nullable:
      given codec: Codec.Nullable[FormData.Value.Nullable, FormData.Value] = Codec.Nullable(
        lift = [A] => (self: Self.Nullable[FormData.Value, A]) => Nullable(self),
        extract = [A] => (codec: FormData.Value.Nullable[A]) => codec.self
      )

    final case class Primitive[A](self: Self.Primitive.String[A]) extends Value[A]

    object Primitive:
      given codec: Codec.Primitive.String[FormData.Value.Primitive] = Codec.Primitive.String(
        lift = [A] => (self: Self.Primitive.String[A]) => Primitive(self),
        extract = [A] => (codec: FormData.Value.Primitive[A]) => codec.self
      )

  type Field[A] = Self.Field[FormData.Key, FormData.Value, A]

  given codec: Codec.Field[FormData.Field, FormData.Key, FormData.Value, FormData.Record] =
    Codec.Field(
      lift = [A] => (self: Self.Field[FormData.Key, FormData.Value, A]) => self,
      extract = [A] => (codec: FormData.Field[A]) => codec
    )
