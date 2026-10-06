package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.data.circe.toJson
import io.taig.otter.Json

object JsonCirceDynamicEncoder:
  def encode[W](schema: Json.Dynamic.Node[W, Any], value: W): CirceJson = schema match
    case _ => schema.write(value)(_.toJson, _.toJson, _.toJson)
