package io.taig.otter.json

import io.circe.Json
import io.circe.JsonObject
import cats.syntax.all.*
import io.circe.syntax.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.*

object BranchJsonDecoder:
  def apply[A](
      branch: Branch.Via[Json, A],
      discriminator: Discriminator,
      json: JsonObject
  ): Decoder.Result[Json, Option[A]] = branch match
    case Branch.Root(_, name, schema) =>
      discriminator match
        case Discriminator.Nested(identifier, value) =>
          json(identifier)
            .toValid(Violations.rootNec(Violation(Constraint.Type("string"), actual = "null".asJson)))
            .andThen: identifier =>
              identifier.asString.toValid(Violations.rootNec(Violation(Constraint.Type("string"), actual = identifier)))
            .leftMap(identifier /: _)
            .map(_ === identifier)
            .andThen:
              case true  => JsonDecoder(schema, json(value).getOrElse(Json.Null)).bimap(value /: _, _.some)
              case false => none.valid
        case Discriminator.Merged(identifier) =>
          json(identifier)
            .toValid(Violations.rootNec(Violation(Constraint.Type("string"), actual = "null".asJson)))
            .andThen: identifier =>
              identifier.asString.toValid(Violations.rootNec(Violation(Constraint.Type("string"), actual = identifier)))
            .leftMap(identifier /: _)
            .map(_ === identifier)
            .andThen:
              case true  => JsonDecoder(schema, Json.fromJsonObject(json.remove(identifier))).map(_.some)
              case false => none.valid
        case Discriminator.Keyed =>
          JsonDecoder(schema, json(name).getOrElse(Json.Null)).bimap(name /: _, _.some)
    case Branch.Transform(self, f, g) => BranchJsonDecoder(self, discriminator, json).map(_.map(f))
