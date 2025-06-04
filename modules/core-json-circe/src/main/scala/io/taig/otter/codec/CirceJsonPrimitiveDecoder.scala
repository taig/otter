package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.circe.Decoder as CirceDecoder
import io.circe.Json as CirceJson
import io.taig.otter.Json
import io.taig.otter.Primitive
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.fromJson

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object CirceJsonPrimitiveDecoder extends Decoder[Primitive[Json.Primitive, *], CirceJson]:
  override def decode[A](schema: Primitive[Json.Primitive, A], json: CirceJson): Validated[Violations, A] =
    decode(schema = schema.value, json)

  def decode[A](schema: Primitive.Value[Json.Primitive, A], json: CirceJson): Validated[Violations, A] = schema match
    case Primitive.Value.Boolean.Root               => decode[Boolean](name = "boolean", json)
    case _: Primitive.Value.Number.BigDecimal       => decode[JBigDecimal](name = "bigDecimal", json)
    case _: Primitive.Value.Number.BigInteger       => decode[JBigInteger](name = "bigInteger", json)
    case _: Primitive.Value.Number.Double           => decode[Double](name = "double", json)
    case _: Primitive.Value.Number.Float            => decode[Float](name = "float", json)
    case _: Primitive.Value.Number.Int              => decode[Int](name = "int", json)
    case _: Primitive.Value.Number.Long             => decode[Long](name = "long", json)
    case Primitive.Value.Boolean.Modify(self, f, _) => decode(schema = self, json).map(f)
    case Primitive.Value.Number.Modify(self, f, _)  => decode(schema = self, json).map(f)
    case Primitive.Value.String.Modify(self, f, _)  => decode(schema = self, json).map(f)
    case Primitive.Value.String.Parsed(self) =>
      decode[String](name = "string", json).andThen(JsonPrimitiveParser.decode(schema = self.value, _))
    case Primitive.Value.String.Parser(name, f, _) =>
      decode[String](name = "string", json).andThen: value =>
        f(value)
          .leftMap(error => Violations.rootNec(Violation.tpe(name, actual = fromJson(json), hint = error)))
          .toValidated
    case _: Primitive.Value.String.Text => decode[String](name = "string", json)

  def decode[A: CirceDecoder](name: String, json: CirceJson): Validated[Violations, A] = json
    .as[A]
    .leftMap(failure => Violations.rootNec(Violation.tpe(name, actual = fromJson(json), hint = failure.show)))
    .toValidated
