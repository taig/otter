package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.Primitive
import io.taig.otter.Violations
import io.taig.otter.typeOf
import io.taig.validation.Violation

object JsonPrimitiveCirceDecoder extends Decoder[Json.Primitive.Read, CirceJson]:
  override def decode[A](schema: Json.Primitive.Read[A], json: CirceJson): Validated[Violations, A] =
    decode(schema = schema.self.self, json)

  def decode[A](schema: Primitive.Read[A], json: CirceJson): Validated[Violations, A] = schema match
    case Primitive.Boolean.Modify(self, f, _) => decode(schema = self, json).map(f)
    case Primitive.Boolean.Root               =>
      json.asBoolean
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "boolean"), actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
    case Primitive.Number.BigDecimal(validation) =>
      json.asNumber
        .flatMap(_.toBigDecimal)
        .map(_.bigDecimal)
        .toValid(Constraint.Generic.Type(name = "bigDecimal"))
        .leftMap(Violation(_, actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .andThen(input => validation.validate(input).toInvalid(input).leftMap(Violations.apply))
    case Primitive.Number.BigInteger(validation) =>
      json.asNumber
        .flatMap(_.toBigInt)
        .map(_.bigInteger)
        .toValid(Constraint.Generic.Type(name = "bigInteger"))
        .leftMap(Violation(_, actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .andThen(input => validation.validate(input).toInvalid(input).leftMap(Violations.apply))
    case Primitive.Number.Double(validation) =>
      json.asNumber
        .map(_.toDouble)
        .toValid(Constraint.Generic.Type(name = "double"))
        .leftMap(Violation(_, actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .andThen(input => validation.validate(input).toInvalid(input).leftMap(Violations.apply))
    case Primitive.Number.Float(validation) =>
      json.asNumber
        .map(_.toFloat)
        .toValid(Constraint.Generic.Type(name = "float"))
        .leftMap(Violation(_, actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .andThen(input => validation.validate(input).toInvalid(input).leftMap(Violations.apply))
    case Primitive.Number.Int(validation) =>
      json.asNumber
        .flatMap(_.toInt)
        .toValid(Constraint.Generic.Type(name = "int"))
        .leftMap(Violation(_, actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .andThen(input => validation.validate(input).toInvalid(input).leftMap(Violations.apply))
    case Primitive.Number.Long(validation) =>
      json.asNumber
        .flatMap(_.toLong)
        .toValid(Constraint.Generic.Type(name = "long"))
        .leftMap(Violation(_, actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .andThen(input => validation.validate(input).toInvalid(input).leftMap(Violations.apply))
    case Primitive.Number.Modify(self, f, _) => decode(schema = self, json).map(f)
    case Primitive.Text.Modify(self, f, _)   => decode(schema = self, json).map(f)
    case Primitive.Text.Codec(_, parse, _)   =>
      json.asString
        .toValid(Constraint.Generic.Type(name = "string"))
        .leftMap(Violation(_, actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .andThen: input =>
          parse(input).toValidated
            .leftMap: error =>
              Violation(
                constraint = Constraint.Generic.Type(name = "string"),
                actual = typeOf(json),
                hint = error.some
              )
            .leftMap(Violations.apply)
    case Primitive.Text.Root(validation) =>
      json.asString
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "string"), actual = typeOf(json), hint = none))
        .leftMap(Violations.apply)
        .andThen: input =>
          validation.validate(input).toInvalid(input).leftMap(Violations.apply)
