package io.taig.otter.json.circe

import io.taig.otter.Tuple
import io.circe.Json
import cats.data.Chain
import cats.data.Validated
import io.taig.otter.validation.Violations

object JsonTupleDecoder:
  def apply[A](schema: Tuple[?, A], json: Option[Chain[Json]]): Validated[Violations, A] =
    ???
