package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.data.circe.toData
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.Primitive
import io.taig.otter.Violations
import io.taig.otter.typeOf
import io.taig.validation.Validation
import io.taig.validation.Violation

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
object JsonPrimitiveCirceDecoder extends Decoder[Json.Primitive.Node, CirceJson]:
  override def decode[R](json: Json.Primitive.Node[Nothing, R], value: CirceJson): Validated[Violations, R] =
    (json: @unchecked) match
      case json: Json.Primitive.Boolean.Of[?, R] => decode(json.self.self, value)
      case json: Json.Primitive.Number.Of[?, R]  => decode(json.self.self, value)
      case json: Json.Primitive.Text.Of[?, R]    => decode(json.self.self, value)

  def decode[R](schema: Primitive[Nothing, R], json: CirceJson): Validated[Violations, R] = (schema: @unchecked) match
    case schema: Primitive.Modify[?, ?, ?, R]         => decode(schema.self, json).map(schema.f)
    case schema: Primitive.Boolean.Modify[?, ?, ?, R] => decode(schema.self, json).map(schema.f)
    case Primitive.Boolean.Root                       =>
      json.asBoolean.toValid(mismatch("boolean", json)).leftMap(Violations.apply).map(_.asInstanceOf[R])
    case Primitive.Number.BigDecimal(validation) =>
      number("bigDecimal", json, _.toBigDecimal.map(_.bigDecimal), validation)
    case Primitive.Number.BigInteger(validation) =>
      number("bigInteger", json, _.toBigInt.map(_.bigInteger), validation)
    case Primitive.Number.Double(validation)         => number("double", json, _.toDouble.some, validation)
    case Primitive.Number.Float(validation)          => number("float", json, _.toFloat.some, validation)
    case Primitive.Number.Int(validation)            => number("int", json, _.toInt, validation)
    case Primitive.Number.Long(validation)           => number("long", json, _.toLong, validation)
    case schema: Primitive.Number.Modify[?, ?, ?, R] => decode(schema.self, json).map(schema.f)
    case schema: Primitive.Text.Codec[?, R]          =>
      text(json).andThen: input =>
        schema
          .parse(input)
          .toValidated
          .leftMap(error => Violation(Constraint.Generic.Type(schema.name), actual = json.toData, hint = error.some))
          .leftMap(Violations.apply)
    case schema: Primitive.Text.Modify[?, ?, ?, R] => decode(schema.self, json).map(schema.f)
    case Primitive.Text.Root(validation)           =>
      text(json)
        .andThen(input => validation.validate(input).toInvalid(input).leftMap(Violations.apply))
        .map(_.asInstanceOf[R])

  private def mismatch(name: String, json: CirceJson): Violation[Constraint] =
    Violation(constraint = Constraint.Generic.Type(name), actual = typeOf(json).asData, hint = none)

  private def text(json: CirceJson): Validated[Violations, String] =
    json.asString.toValid(mismatch("string", json)).leftMap(Violations.apply)

  private def number[A, R](
      name: String,
      json: CirceJson,
      extract: io.circe.JsonNumber => Option[A],
      validation: Validation[Constraint.Primitive.Number, A]
  ): Validated[Violations, R] =
    json.asNumber
      .flatMap(extract)
      .toValid(mismatch(name, json))
      .leftMap(Violations.apply)
      .andThen(input => validation.validate(input).toInvalid(input).leftMap(Violations.apply))
      .map(_.asInstanceOf[R])
