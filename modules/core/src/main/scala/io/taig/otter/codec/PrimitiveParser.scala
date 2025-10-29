package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Constraint
import io.taig.otter.Primitive
import io.taig.otter.Violations
import io.taig.validation.Violation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object PrimitiveParser extends Parser[Primitive]:
  override def decode[A](schema: Primitive[A], value: String): Validated[Violations, A] = schema match
    case Primitive.Boolean.Modify(self, f, _) => decode(schema = self, value).map(f)
    case Primitive.Boolean.Root               =>
      value.toBooleanOption
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "boolean"), actual = value, hint = none))
        .leftMap(Violations.apply)
    case Primitive.Number.BigDecimal(validation) =>
      Validated
        .catchOnly[NumberFormatException](JBigDecimal(value))
        .leftMap: exception =>
          Violation(
            constraint = Constraint.Generic.Type(name = "bigDecimal"),
            actual = value,
            hint = exception.getMessage.some
          )
        .leftMap(Violations.apply)
        .andThen: input =>
          validation
            .validate(input)
            .toInvalid(input)
            .leftMap(Violations.apply)
    case Primitive.Number.BigInteger(validation) =>
      Validated
        .catchOnly[NumberFormatException](JBigInteger(value))
        .leftMap: exception =>
          Violation(
            constraint = Constraint.Generic.Type(name = "bigInteger"),
            actual = value,
            hint = exception.getMessage.some
          )
        .leftMap(Violations.apply)
        .andThen: input =>
          validation
            .validate(input)
            .toInvalid(input)
            .leftMap(Violations.apply)
    case Primitive.Number.Double(validation) =>
      value.toDoubleOption
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "double"), actual = value, hint = none))
        .leftMap(Violations.apply)
        .andThen: input =>
          validation
            .validate(input)
            .toInvalid(input)
            .leftMap(Violations.apply)
    case Primitive.Number.Float(validation) =>
      value.toFloatOption
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "float"), actual = value, hint = none))
        .leftMap(Violations.apply)
        .andThen: input =>
          validation
            .validate(input)
            .toInvalid(input)
            .leftMap(Violations.apply)
    case Primitive.Number.Int(validation) =>
      value.toIntOption
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "int"), actual = value, hint = none))
        .leftMap(Violations.apply)
        .andThen: input =>
          validation
            .validate(input)
            .toInvalid(input)
            .leftMap(Violations.apply)
    case Primitive.Number.Long(validation) =>
      value.toLongOption
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "long"), actual = value, hint = none))
        .leftMap(Violations.apply)
        .andThen: input =>
          validation
            .validate(input)
            .toInvalid(input)
            .leftMap(Violations.apply)
    case Primitive.Number.Modify(self, f, _)      => decode(schema = self, value).map(f)
    case Primitive.String.Modify(self, f, _)      => decode(schema = self, value).map(f)
    case Primitive.String.Parser(name, decode, _) =>
      decode(value).toValidated
        .leftMap: error =>
          Violation(
            constraint = Constraint.Generic.Type(name),
            actual = value,
            hint = error.some
          )
        .leftMap(Violations.apply)
    case Primitive.String.Root(validation) =>
      validation
        .validate(value)
        .toInvalid(value)
        .leftMap(Violations.apply)
