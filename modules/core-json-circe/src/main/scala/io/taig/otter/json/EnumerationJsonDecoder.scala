package io.taig.otter.json

import io.circe.Json
import cats.syntax.all.*
import io.taig.otter.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.enumeration.ext.Mapping

object EnumerationJsonDecoder:
  def apply[A](schema: Enumeration[?, A], json: Json): Decoder.Result[Data, A] = schema match
    case Enumeration.Optional(self) =>
      if json.isNull then None.valid else EnumerationJsonDecoder(self, json).map(_.some)
    case Enumeration.Required.Transform(self, f, _)    => EnumerationJsonDecoder(self, json).map(f)
    case Enumeration.Required.Root(_, schema, mapping) => root(schema, mapping, json)
    case Enumeration.Root(_, schema, mapping)          => root(schema, mapping, json)
    case Enumeration.Transform(self, f, _)             => EnumerationJsonDecoder(self, json).map(f)

  def root[A, B](schema: Value[?, A], mapping: Mapping[B, A], json: Json): Decoder.Result[Data, B] =
    JsonDecoder(schema, json).andThen: a =>
      mapping
        .prj(a)
        .toValid:
          val values = mapping.values.map(mapping.inj).map(JsonEncoder(schema, _)).map(toData)
          Violations.rootNec(Violation(constraint = Constraint.OneOf(values), actual = toData(json)))
