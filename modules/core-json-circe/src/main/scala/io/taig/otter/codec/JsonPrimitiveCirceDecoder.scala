package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.data.circe.*
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.Primitive
import io.taig.otter.Violations
import io.taig.otter.typeOf
import io.taig.validation.Violation

object JsonPrimitiveCirceDecoder extends Decoder[Json.Primitive.Read, CirceJson]:
  override def decode[A](schema: Json.Primitive.Read[A], json: CirceJson): Validated[Violations, A] =
    decode(schema = schema.self.self, json)

  def decode[A](schema: Primitive.Read[Json.Primitive.Read, A], json: CirceJson): Validated[Violations, A] =
    schema match
      case Primitive.Boolean.Modify(self, f, _)   => decode(schema = self, json).map(f)
      case Primitive.Boolean.Read.Modify(self, f) => decode(schema = self, json).map(f)
      case Primitive.Boolean.Root                 =>
        json.asBoolean
          .toValid(
            Violation(constraint = Constraint.Generic.Type(name = "boolean"), actual = typeOf(json), hint = none)
          )
          .leftMap(Violations.apply)
      case Primitive.Coerce.Boolean.Modify(self, f, _)   => decode(schema = self, json).map(f)
      case Primitive.Coerce.Boolean.Read.Modify(self, f) => decode(schema = self, json).map(f)
      case Primitive.Coerce.Boolean.Root(schema)         =>
        val value = json.withString:
          case "true"  => CirceJson.fromBoolean(true)
          case "false" => CirceJson.fromBoolean(false)
          case value   => CirceJson.fromString(value)

        decode(schema = schema.value, value)
      case Primitive.Coerce.Modify(self, f, _)          => decode(schema = self, json).map(f)
      case Primitive.Coerce.Number.Modify(self, f, _)   => decode(schema = self, json).map(f)
      case Primitive.Coerce.Number.Read.Modify(self, f) => decode(schema = self, json).map(f)
      case Primitive.Coerce.Number.Root(schema)         =>
        val value = json.withString: value =>
          schema.value.self.self match
            case Primitive.Number.Int(_) =>
              value.toIntOption.map(CirceJson.fromInt).getOrElse(CirceJson.fromString(value))
            case Primitive.Number.Long(_) =>
              value.toLongOption.map(CirceJson.fromLong).getOrElse(CirceJson.fromString(value))
            case Primitive.Number.Float(_) =>
              value.toFloatOption.flatMap(CirceJson.fromFloat).getOrElse(CirceJson.fromString(value))
            case Primitive.Number.Double(_) =>
              value.toDoubleOption.flatMap(CirceJson.fromDouble).getOrElse(CirceJson.fromString(value))
            case Primitive.Number.BigInteger(_) =>
              Either
                .catchOnly[NumberFormatException](CirceJson.fromBigInt(BigInt(value)))
                .getOrElse(CirceJson.fromString(value))
            case Primitive.Number.BigDecimal(_) =>
              Either
                .catchOnly[NumberFormatException](CirceJson.fromBigDecimal(BigDecimal(value)))
                .getOrElse(CirceJson.fromString(value))
            case _ => CirceJson.fromString(value)

        decode(schema = schema.value, value)
      case Primitive.Coerce.Read.Modify(self, f)      => decode(schema = self, json).map(f)
      case Primitive.Coerce.Text.Modify(self, f, _)   => decode(schema = self, json).map(f)
      case Primitive.Coerce.Text.Read.Modify(self, f) => decode(schema = self, json).map(f)
      case Primitive.Coerce.Text.Root(schema)         =>
        val value = json
          .withBoolean(value => CirceJson.fromString(String.valueOf(value)))
          .withNumber(value => CirceJson.fromString(value.toString))

        decode(schema = schema.value, value)
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
      case Primitive.Number.Modify(self, f, _)   => decode(schema = self, json).map(f)
      case Primitive.Number.Read.Modify(self, f) => decode(schema = self, json).map(f)
      case Primitive.Text.Codec(name, parse, _)  =>
        json.asString
          .toValid(Constraint.Generic.Type(name = "string"))
          .leftMap(Violation(_, actual = typeOf(json), hint = none))
          .leftMap(Violations.apply)
          .andThen: input =>
            parse(input).toValidated
              .leftMap: error =>
                Violation(
                  constraint = Constraint.Generic.Type(name),
                  actual = json.toData,
                  hint = error.some
                )
              .leftMap(Violations.apply)
      case Primitive.Text.Modify(self, f, _)       => decode(schema = self, json).map(f)
      case Primitive.Text.Read.Modify(self, f)     => decode(schema = self, json).map(f)
      case Primitive.Text.Read.Parser(name, parse) =>
        json.asString
          .toValid(Constraint.Generic.Type(name = "string"))
          .leftMap(Violation(_, actual = typeOf(json), hint = none))
          .leftMap(Violations.apply)
          .andThen: input =>
            parse(input).toValidated
              .leftMap: error =>
                Violation(
                  constraint = Constraint.Generic.Type(name),
                  actual = json.toData,
                  hint = error.some
                )
              .leftMap(Violations.apply)
      case Primitive.Text.Root(validation) =>
        json.asString
          .toValid(Violation(constraint = Constraint.Generic.Type(name = "string"), actual = typeOf(json), hint = none))
          .leftMap(Violations.apply)
          .andThen: input =>
            validation.validate(input).toInvalid(input).leftMap(Violations.apply)
