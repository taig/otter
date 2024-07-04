package io.taig.otter.json.circe

import io.taig.otter.Plain.*
import io.taig.otter.Decoder
import io.taig.otter as Base
import cats.data.Chain
import cats.syntax.all.*
import io.circe.Json
import cats.Foldable
import io.taig.otter.validation.Violations
import io.taig.otter.validation.History
import io.taig.otter.validation.Violation
import io.circe.syntax.*

object FieldJsonDecoder:
  def apply[A](
      field: Field.Reader[A],
      values: Option[Chain[(String, Json)]]
  ): (Option[Chain[(String, Json)]], Decoder.Result[Json, A]) = field match
    case Base.Field.Root(name, schema)        => root(name, schema, values)
    case Base.Field.Reader.Root(name, schema) => root(name, schema, values)

  def root[A](
      name: String,
      schema: Schema.Reader[A],
      values: Option[Chain[(String, Json)]]
  ): (Option[Chain[(String, Json)]], Decoder.Result[Json, A]) =
    values match
      case Some(values) =>
        val (json, remainders) = values.findWithRemainders { case (`name`, json) => json }

        json match
          case Some(json) => remainders.some -> JsonDecoder(schema, json).leftMap(name /: _)
          case None =>
            remainders.some -> Violations
              .namespaceNec(
                History.Step.Field(name),
                Violation(Constraint.Type("json"), actual = "null".asJson)
              )
              .invalid
      case None => (values, JsonDecoder(schema, Json.Null).leftMap(name /: _))
