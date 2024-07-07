package io.taig.otter.json.circe

import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json
import cats.syntax.all.*
import io.taig.otter.Decoder

object EnumerationJsonDecoder:
  def apply[A](schema: Enumeration.Reader[A], json: Json): Decoder.Result[Json, A] = schema match
    case Base.Enumeration.Optional(self)                     => optional(self, json)
    case Base.Enumeration.Reader.Optional(self)              => optional(self, json)
    case Base.Enumeration.Reader.Root(self, f)               => root(self, f, json)
    case Base.Enumeration.Reader.Transform(self, f)          => transform(self, f, json)
    case Base.Enumeration.Required.Reader.Root(schema, f)    => root(schema, f, json)
    case Base.Enumeration.Required.Reader.Transform(self, f) => transform(self, f, json)
    case Base.Enumeration.Required.Transform(self, f, _)     => transform(self, f, json)
    case Base.Enumeration.Root(self, mapping)                => root(self, mapping.prj, json)
    case Base.Enumeration.Transform(self, f, _)              => transform(self, f, json)

  def optional[A](self: Enumeration.Reader[A], json: Json): Decoder.Result[Json, Option[A]] =
    if json.isNull then none.valid else EnumerationJsonDecoder(self, json).map(_.some)

  def root[A, B](schema: Value.Reader[A], f: A => Option[B], json: Json): Decoder.Result[Json, B] =
    ??? // JsonDecoder(schema, f(b))

  def transform[A, B](self: Enumeration.Reader[A], f: A => B, json: Json): Decoder.Result[Json, B] =
    EnumerationJsonDecoder(self, json).map(f)
