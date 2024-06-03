package io.taig.otter.json.circe

import io.taig.otter.Schema
import io.taig.otter.+
import io.circe.Json
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter.Plain.*
import cats.syntax.all.*

object JsonUnionDecoder:
  def apply[A](union: Union.Reader[A], json: Json): Validated[Violations[Json, Json], A] = ???
