package io.taig.otter.json

import io.circe.{Json, JsonNumber, JsonObject}
import io.circe.syntax.*
import io.taig.otter.Data
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

def fromData(data: Data.Number): Json = data.value match
  case value: JBigDecimal => value.asJson
  case value: JBigInteger => value.asJson
  case value: Double      => Json.fromDoubleOrString(value)
  case value: Float       => Json.fromFloatOrString(value)
  case value: Int         => Json.fromInt(value)
  case value: Long        => Json.fromLong(value)

def fromData(data: Data.Object[?]): JsonObject =
  JsonObject.fromIterable(data.values.map { case (key, value) => (key, fromData(value)) })

def fromData(data: Data): Json = data match
  case Data.String(value)   => Json.fromString(value)
  case Data.Boolean(value)  => Json.fromBoolean(value)
  case data: Data.Number    => fromData(data)
  case data: Data.Object[?] => Json.fromJsonObject(fromData(data))
  case Data.Array(values)   => Json.fromValues(values.map(fromData))
  case Data.Null            => Json.Null

def toData(json: JsonNumber): Data = json.toInt.map(Data.Number.apply) orElse
  json.toLong.map(Data.Number.apply) orElse
  json.toBigInt.map(value => Data.Number(value.bigInteger)) orElse
  Some(json.toFloat)
    .filter(number => number != Float.NegativeInfinity && number != Float.PositiveInfinity)
    .map(Data.Number.apply) orElse
  Some(json.toDouble)
    .filter(number => number != Double.NegativeInfinity && number != Double.PositiveInfinity)
    .map(Data.Number.apply) orElse
  json.toBigDecimal.map(value => Data.Number(value.bigDecimal)) getOrElse
  Data.Number(json.toDouble)

def toDataArray(json: Vector[Json]): Data.Array[?] = Data.Array(json.map(toData))

def toDataObject(json: JsonObject): Data.Object[?] =
  Data.Object(json.toVector.map { case (key, value) => (key, toData(value)) })

def toData(json: Json): Data = json.fold(
  jsonNull = Data.Null,
  Data.Boolean.apply,
  toData,
  Data.String.apply,
  toDataArray,
  toDataObject
)
