package io.taig.otter.json

import io.taig.otter.*
import cats.data.Chain
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.validation.Violations
import io.taig.otter.validation.History
import io.taig.otter.validation.Violation
import io.circe.syntax.*

object FieldJsonDecoder:
  def apply[A](
      field: Field[?, A],
      values: Option[Chain[(String, Json)]]
  ): Decoder.Result[Json, (Option[Chain[(String, Json)]], A)] = field match
    case Field.Root(_, name, schema) =>
      values match
        case Some(values) =>
          val (json, remainders) = values.findWithRemainders { case (`name`, json) => json }

          json match
            case Some(json) => JsonDecoder(schema, json).leftMap(name /: _).tupleLeft(remainders.some)
            case None =>
              Violations
                .namespaceNec(
                  History.Step.Field(name),
                  Violation(Constraint.Type("json"), actual = "null".asJson)
                )
                .invalid
        case None => JsonDecoder(schema, Json.Null).leftMap(name /: _).tupleLeft(values)
    case Field.Transform(self, f, _) => FieldJsonDecoder(self, values).map(_.map(f))
