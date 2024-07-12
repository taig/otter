package io.taig.otter.json

import io.circe.Json
import io.taig.otter.*
import cats.syntax.all.*

object DynamicJsonDecoder:
  def apply[A](schema: Dynamic.Reader[Json, A], json: Json): Decoder.Result[Json, A] = schema match
    case Dynamic.Root(_)                   => json.valid
    case Dynamic.Reader.Root(_)            => json.valid
    case Dynamic.Reader.Optional(self)     => optional(self, json)
    case Dynamic.Optional(self)            => optional(self, json)
    case Dynamic.Reader.Transform(self, f) => transform(self, f, json)
    case Dynamic.Transform(self, f, _)     => transform(self, f, json)

  def optional[A](self: Dynamic.Reader[Json, A], json: Json): Decoder.Result[Json, Option[A]] =
    if json.isNull then none.valid else DynamicJsonDecoder(self, json).map(_.some)

  def transform[A, B](self: Dynamic.Reader[Json, A], f: A => B, json: Json): Decoder.Result[Json, B] =
    DynamicJsonDecoder(self, json).map(f)
