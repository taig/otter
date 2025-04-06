package io.taig.otter

import cats.data.Validated
import cats.syntax.all.*
import io.circe.Decoder as CirceDecoder
import io.circe.Json as CirceJson

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object CirceJsonDecoder extends Decoder[Json, CirceJson]:
  override def apply[A](codec: Json[A], json: CirceJson): Validated[Violations, A] = codec match
    case Json.Constant(self)  => apply(codec = self, json)
    case Json.Primitive(self) => apply(codec = self, json)

  def apply[A](codec: Constant[Json, A], json: CirceJson): Validated[Violations, A] = codec match
    case Constant.Modify(self, f, _) => apply(codec = self, json).map(f)
    case self @ Constant.Root(codec, _) =>
      apply(codec = codec.self.value, json).andThen: a =>
        Validated
          .cond(
            test = self.matches(a),
            a,
            Violation.equal(
              reference = toValue(CirceJsonEncoder(codec = codec.self.value, codec.value)),
              actual = toValue(json)
            )
          )
          .leftMap(Violations.rootNec)

  def apply[A](codec: Primitive[A], json: CirceJson): Validated[Violations, A] = codec match
    case _: Primitive.Boolean.Root            => apply[Boolean](name = "boolean", json)
    case _: Primitive.Number.BigDecimal       => apply[JBigDecimal](name = "bigDecimal", json)
    case _: Primitive.Number.BigInteger       => apply[JBigInteger](name = "bigInteger", json)
    case _: Primitive.Number.Double           => apply[Double](name = "double", json)
    case _: Primitive.Number.Float            => apply[Float](name = "float", json)
    case _: Primitive.Number.Int              => apply[Int](name = "int", json)
    case _: Primitive.Number.Long             => apply[Long](name = "long", json)
    case Primitive.Boolean.Modify(self, f, _) => apply(codec = self, json).map(f)
    case Primitive.Number.Modify(self, f, _)  => apply(codec = self, json).map(f)
    case Primitive.String.Modify(self, f, _)  => apply(codec = self, json).map(f)
    case Primitive.String.Parser(name, decode, _, _, _, _, _) =>
      apply[String](name = "string", json).andThen: value =>
        decode(value)
          .leftMap(error => Violations.rootNec(Violation.tpe(name, actual = toValue(json), hint = error)))
          .toValidated
    case _: Primitive.String.Text => apply[String](name = "string", json)

  def apply[A: CirceDecoder](name: String, json: CirceJson): Validated[Violations, A] = json
    .as[A]
    .leftMap(failure => Violations.rootNec(Violation.tpe(name, actual = toValue(json), hint = failure.show)))
    .toValidated
