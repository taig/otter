package io.taig.otter.json

import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json
import scala.reflect.ClassTag

object DynamicJsonEncoder:
  def apply[A](schema: Dynamic[Json, A], a: A): Json = schema.encode(a)

