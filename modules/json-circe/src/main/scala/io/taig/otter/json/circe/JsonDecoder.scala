package io.taig.otter.json.circe

import cats.syntax.all.*
import io.circe.Json
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter.Plain.*
import io.taig.otter.Decoder
import io.taig.otter as Base
import io.taig.otter.validation.Violation
import io.circe.syntax.*

object JsonDecoder extends Decoder[Schema.Reader, Json]:
  override def apply[A](schema: Schema.Reader[A], json: Json): Validated[Violations[Json, Json], A] = schema match
    case schema: Collection.Reader[A] => ???
    case schema: Primitive.Reader[A]  => JsonPrimitiveDecoder(schema, json)
    case schema: Tuple.Reader[A]      => ???
