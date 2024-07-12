package io.taig.otter.json

import io.circe.Json
import cats.syntax.all.*
import io.taig.otter.*
import io.taig.enumeration.ext.Mapping
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

object EnumerationJsonDecoder:
  def apply[A](schema: Enumeration.Reader.Via[Json, A], json: Json): Decoder.Result[Json, A] = schema match
    case Enumeration.Optional(self)                                   => optional(self, json)
    case Enumeration.Reader.Optional(self)                            => optional(self, json)
    case Enumeration.Reader.Root(_, schema, mapping, writer)          => root(schema, mapping, writer, json)
    case Enumeration.Reader.Transform(self, f)                        => transform(self, f, json)
    case Enumeration.Required.Reader.Root(_, schema, mapping, writer) => root(schema, mapping, writer, json)
    case Enumeration.Required.Reader.Transform(self, f)               => transform(self, f, json)
    case Enumeration.Required.Transform(self, f, _)                   => transform(self, f, json)
    case Enumeration.Root(_, schema, mapping)                         => root(schema, mapping, schema, json)
    case Enumeration.Transform(self, f, _)                            => transform(self, f, json)

  def optional[A](self: Enumeration.Reader.Via[Json, A], json: Json): Decoder.Result[Json, Option[A]] =
    if json.isNull then none.valid else EnumerationJsonDecoder(self, json).map(_.some)

  def root[A, B](
      schema: Value.Reader.Via[Json, A],
      mapping: Mapping[B, A],
      writer: Schema.Writer.Via[Json, A],
      json: Json
  ): Decoder.Result[Json, B] = JsonDecoder(schema, json).andThen: a =>
    mapping
      .prj(a)
      .toValid:
        val values = mapping.values.map(mapping.inj).map(JsonEncoder(writer, _))
        Violations.rootNec(Violation(constraint = Constraint.OneOf(values), actual = json))

  def transform[A, B](self: Enumeration.Reader.Via[Json, A], f: A => B, json: Json): Decoder.Result[Json, B] =
    EnumerationJsonDecoder(self, json).map(f)
