package io.taig.otter.circe

import cats.data.Validated
import io.circe.Json
import io.taig.otter.validation.Violations
import io.taig.otter.{Decoder, Schema}

object CirceDecoder:
  val schema: Decoder[Schema, Json] = new Decoder[Schema, Json]:
    override def decode[B](schema: Schema[B], a: Json): Validated[Violations, B] = ???
