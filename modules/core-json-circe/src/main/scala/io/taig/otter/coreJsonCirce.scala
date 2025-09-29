package io.taig.otter
import io.circe.Json

private[otter] def typeOf(json: Json): String = json.fold(
  jsonNull = "null",
  jsonBoolean = _ => "boolean",
  jsonNumber = _ => "number",
  jsonString = _ => "string",
  jsonArray = _ => "array",
  jsonObject = _ => "object"
)
