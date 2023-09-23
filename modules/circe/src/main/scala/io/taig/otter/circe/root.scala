package io.taig.otter.circe

import cats.data.Chain
import io.circe.{Json, JsonNumber}
import io.taig.otter.Data

def fromData(data: Data.Number): Json = data.value match
  case value: BigDecimal => Json.fromBigDecimal(value)
  case value: BigInt     => Json.fromBigInt(value)
  case value: Double     => Json.fromDoubleOrString(value)
  case value: Float      => Json.fromFloatOrString(value)
  case value: Int        => Json.fromInt(value)
  case value: Long       => Json.fromLong(value)

def fromData(data: Data): Json = data match
  case Data.String(value)  => Json.fromString(value)
  case Data.Boolean(value) => Json.fromBoolean(value)
  case data: Data.Number   => fromData(data)
  case Data.Object(values) => Json.fromFields(values.map { case (key, value) => (key, fromData(value)) }.toList)
  case Data.Array(values)  => Json.fromValues(values.map(fromData).toVector)
  case Data.Null           => Json.Null

def toData(json: JsonNumber): Data = json.toInt.map(Data.Number.apply) orElse
  json.toLong.map(Data.Number.apply) orElse
  json.toBigInt.map(Data.Number.apply) orElse
  Some(json.toFloat)
    .filter(number => number != Float.NegativeInfinity && number != Float.PositiveInfinity)
    .map(Data.Number.apply) orElse
  Some(json.toDouble)
    .filter(number => number != Double.NegativeInfinity && number != Double.PositiveInfinity)
    .map(Data.Number.apply) orElse
  json.toBigDecimal.map(Data.Number.apply) getOrElse
  Data.Number(json.toDouble)

def toData(json: Json): Data = json.fold(
  jsonNull = Data.Null,
  Data.Boolean.apply,
  toData,
  Data.String.apply,
  values => Data.Array(Chain.fromSeq(values.map(toData))),
  values => Data.Object(Chain.fromIterableOnce(values.toIterable).map { case (key, value) => (key, toData(value)) })
)
