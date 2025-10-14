package io.taig.otter

import io.circe.Json as CirceJson

private[otter] def typeOf(json: CirceJson): String = json.fold(
  jsonNull = "null",
  jsonBoolean = _ => "boolean",
  jsonNumber = _ => "number",
  jsonString = _ => "string",
  jsonArray = _ => "array",
  jsonObject = _ => "object"
)
