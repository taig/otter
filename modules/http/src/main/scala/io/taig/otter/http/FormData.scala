package io.taig.otter.http

import io.taig.otter as Self
import Self.Codec

sealed abstract class FormData[A] extends Product with Serializable

object FormData:
  final case class Primitive[A](self: Self.Primitive.String[A]) extends FormData[A]

  object Primitive:
    given codec: Codec.Primitive.String[FormData.Primitive] = Codec.Primitive.String(
      lift = [A] => (self: Self.Primitive.String[A]) => Primitive(self),
      extract = [A] => (codec: FormData.Primitive[A]) => codec.self
    )

  final case class Record[A](self: Self.Record[FormData.Key, FormData, A]) extends FormData[A]

  object Record:
    given codec: Codec.Record[FormData.Record, FormData.Key, FormData] = Codec.Record(
      lift = [A] => (self: Self.Record[FormData.Key, FormData, A]) => Record(self),
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
