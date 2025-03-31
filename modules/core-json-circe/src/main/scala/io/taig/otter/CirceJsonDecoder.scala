package io.taig.otter

import io.circe.Json as CirceJson
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import cats.syntax.all.*
import io.circe.Decoder

object CirceJsonDecoder:
  def apply[A](codec: Json[A], json: CirceJson): Either[Violations, A] = codec.self match
    case codec: Primitive[A] => apply(codec, json)

  def apply[A](codec: Primitive[A], json: CirceJson): Either[Violations, A] = codec match
    case _: Primitive.BigDecimal      => lift[JBigDecimal](name = "bigDecimal", json)
    case _: Primitive.BigInteger      => lift[JBigInteger](name = "bigInteger", json)
    case _: Primitive.Boolean         => lift[Boolean](name = "boolean", json)
    case _: Primitive.Double          => lift[Double](name = "double", json)
    case _: Primitive.Float           => lift[Float](name = "float", json)
    case _: Primitive.Int             => lift[Int](name = "int", json)
    case _: Primitive.Long            => lift[Long](name = "long", json)
    case Primitive.Modify(self, f, _) => apply(codec = self, json).map(f)
    case Primitive.Parser(name, decode, _, _, _, _, _) =>
      lift[String](name = "string", json).flatMap: value =>
        decode(value).leftMap(error => Violations.rootNec(Violation.tpe(name, actual = toValue(json), hint = error)))
    case _: Primitive.String =>
      lift[String](name = "string", json)

  private def lift[A: Decoder](name: String, json: CirceJson) = json
    .as[A]
    .leftMap(failure => Violations.rootNec(Violation.tpe(name, actual = toValue(json), hint = failure.show)))
