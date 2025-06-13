package io.taig.otter

import cats.syntax.all.*
import io.circe.Json
import io.circe.JsonObject

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.annotation.targetName

def fromJson(json: Json): Data = json.fold(
  jsonNull = Data.Null,
  jsonBoolean = identity,
  jsonNumber = number =>
    number.toInt
      .orElse(number.toLong)
      .orElse(number.toFloat.some.filter(value => value != Float.NegativeInfinity && value != Float.PositiveInfinity))
      .orElse(
        number.toDouble.some.filter(value => value != Double.NegativeInfinity && value != Double.PositiveInfinity)
      )
      .orElse(number.toBigInt.map(_.bigInteger))
      .orElse(number.toBigDecimal.map(_.bigDecimal))
      .getOrElse(number.toDouble),
  jsonString = identity,
  jsonArray = fromJsonArray,
  jsonObject = fromJsonObject
)

def fromJsonObject(json: JsonObject): Data.Object[Data] =
  Data.Object(json.toList.map(_.map(fromJson)))

def fromJsonArray(json: Vector[Json]): Data.Array[Data] =
  Data.Array(json.map(fromJson))

def toJson(data: Data): Json = data match
  case data: Data.Primitive    => toJson(data)
  case data: Data.Object[Data] => Json.fromJsonObject(toJsonObject(data))
  case data: Data.Array[Data]  => Json.fromValues(toJsonArray(data))
  case Data.Null               => Json.Null

@targetName("toJsonPrimitive")
def toJson(data: Data.Primitive): Json = data match
  case value: Long        => Json.fromLong(value)
  case value: Int         => Json.fromInt(value)
  case value: Float       => Json.fromFloatOrString(value)
  case value: Double      => Json.fromDoubleOrString(value)
  case value: JBigDecimal => Json.fromBigDecimal(BigDecimal(value))
  case value: JBigInteger => Json.fromBigInt(BigInt(value))
  case value: Boolean     => Json.fromBoolean(value)
  case value: String      => Json.fromString(value)

def toJsonObject(data: Data.Object[Data]): JsonObject =
  JsonObject.fromIterable(data.values.map { case (key, value) => key -> toJson(value) })

def toJsonArray(data: Data.Array[Data]): Vector[Json] = data.values.map(toJson)

private[otter] def typeOf(json: Json): String = json.fold(
  jsonNull = "null",
  jsonBoolean = _ => "boolean",
  jsonNumber = _ => "number",
  jsonString = _ => "string",
  jsonArray = _ => "array",
  jsonObject = _ => "object"
)
