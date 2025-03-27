package io.taig.otter

import io.circe.Json
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import cats.syntax.all.*

object CirceJsonPrimitiveParser:
  def apply[A](codec: Primitive[?, A], json: Json): Either[Violations, A] = codec match
    case _: Primitive.BigDecimal => json.as[JBigDecimal]
      .leftMap(_ => Violations.rootNec(Violation.tpe(name = "bigDecimal", actual = json.noSpaces)))
    case _: Primitive.BigInteger =>  json.as[JBigInteger]
      .leftMap(_ => Violations.rootNec(Violation.tpe(name = "bigInteger", actual = json.noSpaces)))
    case _: Primitive.Boolean => json.as[Boolean]
      .leftMap(_ => Violations.rootNec(Violation.tpe(name = "boolean", actual = json.noSpaces)))
    case _: Primitive.Double => ???
    case _: Primitive.Float => ???
    case _: Primitive.Int => ???
    case _: Primitive.Long => ???
    case Primitive.Modify(self, f, g) => ???
    case Primitive.Parser(name, decode, encode, minimum, maximum, matches, metadata) => ???
    case _: Primitive.String => ???

  def toValue(json: Json): Option[Data.Any] = json.fold(
    jsonNull = none,
    jsonBoolean = _.some,
    jsonNumber = ???,
    jsonString = _.some,
    jsonArray = values => Data.Array(values.map(toValue)).some,
    jsonObject = ???
  )
  