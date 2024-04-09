package io.taig.otter.json.circe

import cats.syntax.all.*
import io.taig.otter.*
import io.circe.Json
import cats.data.Validated
import io.taig.otter.validation.Violations
import cats.data.Chain
import io.taig.otter.validation.Violation
import io.taig.otter.validation.Constraint
import io.circe.syntax.*

object JsonDecoder extends Decoder[Json]:
  def decode[A](schema: Primitive[A], json: Json): Validated[Violations[Json], A] =
    JsonPrimitiveDecoder.decode(schema, json)

  def decode[A](schema: Tuple[A], json: Json): Validated[Violations[Json], A] =
    if json.isNull then JsonTupleDecoder.decode(schema, none)
    else
      json.asArray match
        case Some(values) => JsonTupleDecoder.decode(schema, Chain.fromSeq(values).some)
        case None         => Violations.rootNec(Violation(Constraint.Type("array"), typeOf(json).asJson)).invalid
