package io.taig.otter.json

import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json
import io.taig.otter.Decoder
import cats.syntax.all.*

object DynamicJsonDecoder:
  def apply[A](schema: Dynamic.Reader[Json, A], json: Json): Decoder.Result[Json, A] = schema match
    case Base.Dynamic.Root()                    => json.valid
    case Base.Dynamic.Reader.Root()             => json.valid
    case Base.Dynamic.Reader.Optional(self)     => optional(self, json)
    case Base.Dynamic.Optional(self)            => optional(self, json)
    case Base.Dynamic.Reader.Transform(self, f) => transform(self, f, json)
    case Base.Dynamic.Transform(self, f, _)     => transform(self, f, json)

  def optional[A](self: Dynamic.Reader[Json, A], json: Json): Decoder.Result[Json, Option[A]] =
    if json.isNull then none.valid else DynamicJsonDecoder(self, json).map(_.some)

  def transform[A, B](self: Dynamic.Reader[Json, A], f: A => B, json: Json): Decoder.Result[Json, B] =
    DynamicJsonDecoder(self, json).map(f)
