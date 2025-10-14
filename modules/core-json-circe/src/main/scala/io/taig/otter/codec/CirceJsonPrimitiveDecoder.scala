package io.taig.otter.codec

import io.taig.otter as Self
import io.taig.otter.Json
import io.circe.Json as CirceJson
import cats.data.Validated
import io.taig.otter.Violations
import cats.syntax.all.*
import io.taig.otter.typeOf
import Self.Violation
import Self.Constraint

object CirceJsonPrimitiveDecoder extends Decoder[Json.Primitive, CirceJson]:
  override def decode[A](schema: Json.Primitive[A], json: CirceJson): Validated[Violations, A] =
    decode(schema = schema.self.self, json)

  def decode[A](schema: Self.Primitive[A], json: CirceJson): Validated[Violations, A] = schema match
    case Self.Primitive.Boolean.Modify(self, f, _) => decode(schema = self, json).map(f)
    case Self.Primitive.Boolean.Root               =>
      json.asBoolean
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "boolean"), actual = typeOf(json)))
        .leftMap(Violations.apply)
    case Self.Primitive.Number.BigDecimal(validation) =>
      json.asNumber
        .flatMap(_.toBigDecimal)
        .map(_.bigDecimal)
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "bigDecimal"), actual = typeOf(json)))
        .leftMap(Violations.apply)
        .andThen: input =>
          validation
            .validate(input)
            .toValidated
            .as(input)
            .leftMap(_.map(Violation(_, actual = typeOf(json))))
            .leftMap(Violations.apply)
    case Self.Primitive.Number.BigInteger(validation) =>
      json.asNumber
        .flatMap(_.toBigInt)
        .map(_.bigInteger)
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "bigInteger"), actual = typeOf(json)))
        .leftMap(Violations.apply)
        .andThen: input =>
          validation
            .validate(input)
            .toValidated
            .as(input)
            .leftMap(_.map(Violation(_, actual = typeOf(json))))
            .leftMap(Violations.apply)
    case Self.Primitive.Number.Double(validation) =>
      json.asNumber
        .map(_.toDouble)
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "double"), actual = typeOf(json)))
        .leftMap(Violations.apply)
        .andThen: input =>
          validation
            .validate(input)
            .toValidated
            .as(input)
            .leftMap(_.map(Violation(_, actual = typeOf(json))))
            .leftMap(Violations.apply)
    case Self.Primitive.Number.Float(validation) =>
      json.asNumber
        .map(_.toFloat)
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "float"), actual = typeOf(json)))
        .leftMap(Violations.apply)
        .andThen: input =>
          validation
            .validate(input)
            .toValidated
            .as(input)
            .leftMap(_.map(Violation(_, actual = typeOf(json))))
            .leftMap(Violations.apply)
    case Self.Primitive.Number.Int(validation) =>
      json.asNumber
        .flatMap(_.toInt)
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "int"), actual = typeOf(json)))
        .leftMap(Violations.apply)
        .andThen: input =>
          validation
            .validate(input)
            .toValidated
            .as(input)
            .leftMap(_.map(Violation(_, actual = typeOf(json))))
            .leftMap(Violations.apply)
    case Self.Primitive.Number.Long(validation) =>
      json.asNumber
        .flatMap(_.toLong)
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "long"), actual = typeOf(json)))
        .leftMap(Violations.apply)
        .andThen: input =>
          validation
            .validate(input)
            .toValidated
            .as(input)
            .leftMap(_.map(Violation(_, actual = typeOf(json))))
            .leftMap(Violations.apply)
    case Self.Primitive.Number.Modify(self, f, _)   => decode(schema = self, json).map(f)
    case Self.Primitive.String.Modify(self, f, _)   => decode(schema = self, json).map(f)
    case Self.Primitive.String.Parser(_, decode, _) =>
      json.asString
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "string"), actual = typeOf(json)))
        .leftMap(Violations.apply)
        .andThen: input =>
          decode(input).toValidated
            .leftMap: error =>
              Violation(
                constraint = Constraint.Generic.Type(name = "string"),
                actual = typeOf(json),
                hint = error.some
              )
            .leftMap(Violations.apply)
    case Self.Primitive.String.Root(validation) =>
      json.asString
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "string"), actual = typeOf(json)))
        .leftMap(Violations.apply)
        .andThen: input =>
          validation
            .validate(input)
            .toValidated
            .as(input)
            .leftMap(_.map(Violation(_, actual = typeOf(json))))
            .leftMap(Violations.apply)
