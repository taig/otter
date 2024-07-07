package io.taig.otter.json.circe

import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json
import cats.syntax.all.*
import io.taig.otter.Decoder
import io.taig.enumeration.ext.Mapping
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

object EnumerationJsonDecoder:
  def apply[A](schema: Enumeration.Reader[A], json: Json): Decoder.Result[Json, A] = schema match
    case Base.Enumeration.Optional(self)                                => optional(self, json)
    case Base.Enumeration.Reader.Optional(self)                         => optional(self, json)
    case Base.Enumeration.Reader.Root(self, mapping, writer)            => root(self, mapping, writer, json)
    case Base.Enumeration.Reader.Transform(self, f)                     => transform(self, f, json)
    case Base.Enumeration.Required.Reader.Root(schema, mapping, writer) => root(schema, mapping, writer, json)
    case Base.Enumeration.Required.Reader.Transform(self, f)            => transform(self, f, json)
    case Base.Enumeration.Required.Transform(self, f, _)                => transform(self, f, json)
    case Base.Enumeration.Root(self, mapping)                           => root(self, mapping, self, json)
    case Base.Enumeration.Transform(self, f, _)                         => transform(self, f, json)

  def optional[A](self: Enumeration.Reader[A], json: Json): Decoder.Result[Json, Option[A]] =
    if json.isNull then none.valid else EnumerationJsonDecoder(self, json).map(_.some)

  def root[A, B](
      schema: Value.Reader[A],
      mapping: Mapping[B, A],
      writer: Schema.Writer[A],
      json: Json
  ): Decoder.Result[Json, B] = JsonDecoder(schema, json).andThen: a =>
      mapping
        .prj(a)
        .toValid:
          val values = mapping.values.map(mapping.inj).map(JsonEncoder(writer, _))
          Violations.rootNec(Violation(constraint = Constraint.OneOf(values), actual = json))

  def transform[A, B](self: Enumeration.Reader[A], f: A => B, json: Json): Decoder.Result[Json, B] =
    EnumerationJsonDecoder(self, json).map(f)
