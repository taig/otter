package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.data.circe.toData
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.JsonCirce
import io.taig.otter.Primitive
import io.taig.otter.Violations
import io.taig.validation.Validation
import io.taig.validation.Violation

object JsonPrimitiveCirceDecoder extends Decoder[Json.Primitive.Node, CirceJson]:
  override def decode[R](json: Json.Primitive.Node[Nothing, R], value: CirceJson): Validated[Violations, R] =
    json match
      case Json.Primitive.Boolean.Schema(annotation) => decode(annotation.self, value)
      case Json.Primitive.Number.Schema(annotation)  => decode(annotation.self, value)
      case Json.Primitive.Text.Schema(annotation)    => decode(annotation.self, value)

  def decode[R](schema: Primitive[Nothing, R], json: CirceJson): Validated[Violations, R] = schema match
    case Primitive.Modify(self, f, _)         => decode(self, json).map(f)
    case Primitive.Boolean.Modify(self, f, _) => decode(self, json).map(f)
    case Primitive.Boolean.Root               =>
      json.asBoolean.toValid(mismatch("boolean", json)).leftMap(Violations.apply)
    case Primitive.Number.BigDecimal(validation) =>
      number("bigDecimal", json, _.toBigDecimal.map(_.bigDecimal), validation)
    case Primitive.Number.BigInteger(validation) =>
      number("bigInteger", json, _.toBigInt.map(_.bigInteger), validation)
    case Primitive.Number.Double(validation)   => number("double", json, _.toDouble.some, validation)
    case Primitive.Number.Float(validation)    => number("float", json, _.toFloat.some, validation)
    case Primitive.Number.Int(validation)      => number("int", json, _.toInt, validation)
    case Primitive.Number.Long(validation)     => number("long", json, _.toLong, validation)
    case Primitive.Number.Modify(self, f, _)   => decode(self, json).map(f)
    case Primitive.Text.Format(name, parse, _) =>
      text(json).andThen: input =>
        parse(input).toValidated
          .leftMap(error => Violation(Constraint.Generic.Type(name), actual = json.toData, hint = error.some))
          .leftMap(Violations.apply)
    case Primitive.Text.Modify(self, f, _) => decode(self, json).map(f)
    case Primitive.Text.Root(validation)   =>
      text(json).andThen(input => validation.validate(input).toInvalid(input).leftMap(Violations.apply))

  private def mismatch(name: String, json: CirceJson): Violation[Constraint] =
    Violation(constraint = Constraint.Generic.Type(name), actual = JsonCirce.typeOf(json).asData, hint = none)

  private def text(json: CirceJson): Validated[Violations, String] =
    json.asString.toValid(mismatch("string", json)).leftMap(Violations.apply)

  private def number[A](
      name: String,
      json: CirceJson,
      extract: io.circe.JsonNumber => Option[A],
      validation: Validation[Constraint.Primitive.Number, A]
  ): Validated[Violations, A] =
    json.asNumber
      .flatMap(extract)
      .toValid(mismatch(name, json))
      .leftMap(Violations.apply)
      .andThen(input => validation.validate(input).toInvalid(input).leftMap(Violations.apply))
