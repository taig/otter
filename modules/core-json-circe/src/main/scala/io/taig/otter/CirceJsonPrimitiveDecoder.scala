package io.taig.otter

import io.taig.otter.codec.Decoder
import io.circe.Json as CirceJson
import io.circe.Decoder as CirceDecoder
import cats.data.Validated
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import cats.syntax.all.*

object CirceJsonPrimitiveDecoder extends Decoder[Primitive, CirceJson]:
  override def decode[A](schema: Primitive[A], json: CirceJson): Validated[Violations, A] = schema match
    case _: Primitive.Boolean.Root            => decode[Boolean](name = "boolean", json)
    case _: Primitive.Number.BigDecimal       => decode[JBigDecimal](name = "bigDecimal", json)
    case _: Primitive.Number.BigInteger       => decode[JBigInteger](name = "bigInteger", json)
    case _: Primitive.Number.Double           => decode[Double](name = "double", json)
    case _: Primitive.Number.Float            => decode[Float](name = "float", json)
    case _: Primitive.Number.Int              => decode[Int](name = "int", json)
    case _: Primitive.Number.Long             => decode[Long](name = "long", json)
    case Primitive.Boolean.Modify(self, f, _) => decode(schema = self, json).map(f)
    case Primitive.Number.Modify(self, f, _)  => decode(schema = self, json).map(f)
    case Primitive.String.Modify(self, f, _)  => decode(schema = self, json).map(f)
    case Primitive.String.Parser(name, f, _, _, _, _, _) =>
      decode[String](name = "string", json).andThen: value =>
        f(value)
          .leftMap(error => Violations.rootNec(Violation.tpe(name, actual = toValue(json), hint = error)))
          .toValidated
    case _: Primitive.String.Text => decode[String](name = "string", json)

  def decode[A: CirceDecoder](name: String, json: CirceJson): Validated[Violations, A] = json
    .as[A]
    .leftMap(failure => Violations.rootNec(Violation.tpe(name, actual = toValue(json), hint = failure.show)))
    .toValidated
