package io.taig.otter.json.circe

import io.taig.otter.Type
import io.circe.Json

def typeOf(tpe: Type[?]): String = tpe match
  case Type.BigDecimal | Type.BigInteger | Type.Double | Type.Float | Type.Int | Type.Long => "number"
  case Type.Boolean                                                                        => "boolean"
  case Type.String                                                                         => "string"

def typeOf(json: Json): String = json.fold(
  "null",
  _ => "boolean",
  _ => "number",
  _ => "string",
  _ => "array",
  _ => "object"
)
