package io.taig.otter.json.circe

import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json
import io.circe.JsonObject
import cats.syntax.all.*
import io.taig.otter.Sum.Discriminator
import io.circe.syntax.*
import io.taig.otter.Decoder
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation

object BranchJsonDecoder:
  def apply[A](
      branch: Branch.Reader[A],
      disciminator: Sum.Discriminator,
      json: JsonObject
  ): Decoder.Result[Json, Option[A]] = branch match
    case Base.Branch.Root(name, schema)        => root(name, schema, disciminator, json)
    case Base.Branch.Reader.Root(name, schema) => root(name, schema, disciminator, json)

  def root[A](
      name: String,
      schema: Schema.Reader[A],
      discriminator: Sum.Discriminator,
      json: JsonObject
  ): Decoder.Result[Json, Option[A]] = discriminator match
    case Sum.Discriminator.Nested(identifier, value) =>
      json(identifier)
        .toValid(Violations.rootNec(Violation(Constraint.Type("string"), actual = "null".asJson)))
        .andThen: identifier =>
          identifier.asString.toValid(Violations.rootNec(Violation(Constraint.Type("string"), actual = identifier)))
        .leftMap(identifier /: _)
        .map(_ === identifier)
        .andThen:
          case true  => JsonDecoder(schema, json(value).getOrElse(Json.Null)).bimap(value /: _, _.some)
          case false => none.valid
    case Sum.Discriminator.Merged(identifier) =>
      json(identifier)
        .toValid(Violations.rootNec(Violation(Constraint.Type("string"), actual = "null".asJson)))
        .andThen: identifier =>
          identifier.asString.toValid(Violations.rootNec(Violation(Constraint.Type("string"), actual = identifier)))
        .leftMap(identifier /: _)
        .map(_ === identifier)
        .andThen:
          case true  => JsonDecoder(schema, Json.fromJsonObject(json.remove(identifier))).map(_.some)
          case false => none.valid
    case Sum.Discriminator.Keyed =>
      JsonDecoder(schema, json(name).getOrElse(Json.Null)).bimap(name /: _, _.some)
