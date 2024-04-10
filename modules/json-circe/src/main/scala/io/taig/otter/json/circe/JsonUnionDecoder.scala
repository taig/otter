package io.taig.otter.json.circe

import io.taig.otter.Schema
import io.taig.otter.+
import io.circe.Json
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter.Union
import cats.syntax.all.*

object JsonUnionDecoder:
  def decode[A](union: Union[Schema[?], A], json: Json): Validated[Violations[Json], A] = union match
    case Union.One(schema)         => JsonDecoder.decode(schema, json)
    case Union.OrElse(left, right) => decodeOrElse(left, right, json)
    case Union.Optional(schema) =>
      if json.isNull then none.valid[Violations[Json]] else decode(schema, json).map(_.some)
    case Union.Validate(schema, constraint, validation, _) =>
      decode(schema, json).andThen: a =>
        validation(a)
          .leftMap(_.map(_.map(JsonEncoder.encode(constraint, _))))
          .leftMap(Violations.root)

  def decodeOrElse[A, B](
      left: Union[Schema[?], A],
      right: Union[Schema[?], B],
      json: Json
  ): Validated[Violations[Json], A + B] = decode(left, json).map(_.asLeft).findValid(decode(right, json).map(_.asRight))
