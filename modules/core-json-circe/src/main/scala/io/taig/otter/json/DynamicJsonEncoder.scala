package io.taig.otter.json

import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json
import scala.reflect.ClassTag

object DynamicJsonEncoder:
  def apply[A: ClassTag](schema: Dynamic.Writer[A], a: A): Json = schema match
    case _: Base.Dynamic.Root[Json] => ???
    case _: Base.Dynamic.Root[Any]  => ???
