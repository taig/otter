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