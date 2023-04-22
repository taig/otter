package io.taig.openapi

import cats.syntax.all.*
import io.circe.{Json, JsonObject}

object circe:
  def toOpenApi(json: Json): OpenApi = json.fold(
    jsonNull = OpenApi.Null,
    jsonBoolean = OpenApi.fromBoolean,
    jsonNumber = { number =>
      number.toInt.map(OpenApi.fromInt) orElse
        number.toLong.map(OpenApi.fromLong) orElse
        number.toBigInt.map(OpenApi.fromBigInt) orElse
        Some(number.toFloat)
          .filter(number => number != Float.NegativeInfinity && number != Float.PositiveInfinity)
          .map(OpenApi.fromFloat) orElse
        Some(number.toDouble)
          .filter(number => number != Double.NegativeInfinity && number != Double.PositiveInfinity)
          .map(OpenApi.fromDouble) orElse
        number.toBigDecimal.map(OpenApi.fromBigDecimal) getOrElse
        OpenApi.fromDouble(number.toDouble)
    },
    jsonString = OpenApi.fromString,
    jsonArray = toOpenApiArray,
    jsonObject = toOpenApiObject
  )

  def toOpenApiArray(json: Vector[Json]): OpenApi.Array[OpenApi] = OpenApi.Array(json.map(toOpenApi))

  def toOpenApiObject(json: JsonObject): OpenApi.Object = OpenApi.Object(json.toMap.fmap(toOpenApi))

  def toJson(openapi: OpenApi): Json = openapi match
    case OpenApi.Array(values)     => Json.fromValues(values.map(toJson))
    case OpenApi.BigDecimal(value) => Json.fromBigDecimal(value)
    case OpenApi.BigInt(value)     => Json.fromBigInt(value)
    case OpenApi.Boolean(value)    => Json.fromBoolean(value)
    case OpenApi.Double(value)     => Json.fromDoubleOrString(value)
    case OpenApi.Float(value)      => Json.fromFloatOrString(value)
    case OpenApi.Int(value)        => Json.fromInt(value)
    case OpenApi.Long(value)       => Json.fromLong(value)
    case OpenApi.Null              => Json.Null
    case OpenApi.Object(values)    => Json.fromFields(values.fmap(toJson))
    case OpenApi.String(value)     => Json.fromString(value)
