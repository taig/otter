package io.taig.otter

import cats.syntax.all.*
import io.circe.Json

private[otter] def toValue(json: Json): Data = json.fold(
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
  jsonArray = values => Data.Array(values.map(toValue)),
  jsonObject = values => Data.Object(values.toList.map(_.map(toValue)))
)
