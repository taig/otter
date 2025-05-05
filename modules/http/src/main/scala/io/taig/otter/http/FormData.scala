package io.taig.otter.http

import io.taig.otter as Self
import Self.Codec

sealed abstract class FormData[A] extends Product with Serializable

object FormData:
  final case class Dictionary[A](self: Self.Dictionary[FormData.Key, FormData.Value, A]) extends FormData[A]

  object Dictionary:
    given codec: Codec.Dictionary[FormData.Dictionary, FormData.Key, FormData.Value] = Codec.Dictionary(
      lift = [A] => (self: Self.Dictionary[FormData.Key, FormData.Value, A]) => Dictionary(self),
      extract = [A] => (codec: FormData.Dictionary[A]) => codec.self
    )

  final case class Record[A](self: Self.Record[FormData.Key, FormData.Value, A]) extends FormData[A]

  object Record:
    given codec: Codec.Record[FormData.Record, FormData.Key, FormData.Value] = Codec.Record(
      lift = [A] => (self: Self.Record[FormData.Key, FormData.Value, A]) => Record(self),
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
