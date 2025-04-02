package io.taig.otter

import cats.data.Validated
import io.circe.Json as CirceJson
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import cats.syntax.all.*
import io.circe.Decoder

object CirceJsonDecoder:
  def apply[A](codec: Json[A], json: CirceJson): Validated[Violations, A] = codec.value match
    case codec: Primitive[A] => apply(codec, json)

  def apply[A](codec: Constant[Json, A], json: CirceJson): Validated[Violations, A] = codec match
    case Constant.Modify(self, f, _) => apply(codec = self, json).map(f)
    case self @ Constant.Root(codec, reference, _) =>
      apply(codec = codec.value, json).andThen: a =>
        Validated.cond(
          test = self.matches(a),
          a,
          Violations.rootNec(
            Violation.equal(
              reference = toValue(CirceJsonEncoder(codec = codec.value, reference)),
              actual = toValue(json)
            )
          )
        )

  def apply[A](codec: Primitive[A], json: CirceJson): Validated[Violations, A] = codec match
    case _: Primitive.Boolean.Root            => lift[Boolean](name = "boolean", json)
    case _: Primitive.Number.BigDecimal       => lift[JBigDecimal](name = "bigDecimal", json)
    case _: Primitive.Number.BigInteger       => lift[JBigInteger](name = "bigInteger", json)
    case _: Primitive.Number.Double           => lift[Double](name = "double", json)
    case _: Primitive.Number.Float            => lift[Float](name = "float", json)
    case _: Primitive.Number.Int              => lift[Int](name = "int", json)
    case _: Primitive.Number.Long             => lift[Long](name = "long", json)
    case Primitive.Boolean.Modify(self, f, _) => apply(codec = self, json).map(f)
    case Primitive.Number.Modify(self, f, _)  => apply(codec = self, json).map(f)
    case Primitive.String.Modify(self, f, _)  => apply(codec = self, json).map(f)
    case Primitive.String.Parser(name, decode, _, _, _, _, _) =>
      lift[String](name = "string", json).andThen: value =>
        decode(value)
          .leftMap(error => Violations.rootNec(Violation.tpe(name, actual = toValue(json), hint = error)))
          .toValidated
    case _: Primitive.String.Text => lift[String](name = "string", json)

  private def lift[A: Decoder](name: String, json: CirceJson): Validated[Violations, A] = json
    .as[A]
    .leftMap(failure => Violations.rootNec(Violation.tpe(name, actual = toValue(json), hint = failure.show)))
    .toValidated
