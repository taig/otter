package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Constraint
import io.taig.otter.Primitive
import io.taig.otter.Violation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import io.taig.otter.Violations

object PrimitiveParser extends Parser[Primitive]:
  override def parse[A](schema: Primitive[A], value: String): Validated[Violations, A] = schema match
    case Primitive.Boolean.Modify(self, f, _) => parse(schema = self, value).map(f)
    case Primitive.Boolean.Root               =>
      value.toBooleanOption
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "boolean"), actual = value))
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
            .toValidated
            .as(input)
            .leftMap(_.map(Violation(_, actual = value)))
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
            .toValidated
            .as(input)
            .leftMap(_.map(Violation(_, actual = value)))
            .leftMap(Violations.apply)
    case Primitive.Number.Double(validation) =>
      value.toDoubleOption
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "double"), actual = value))
        .leftMap(Violations.apply)
        .andThen: input =>
          validation
            .validate(input)
            .toValidated
            .as(input)
            .leftMap(_.map(Violation(_, actual = value)))
            .leftMap(Violations.apply)
    case Primitive.Number.Float(validation) =>
      value.toFloatOption
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "float"), actual = value))
        .leftMap(Violations.apply)
        .andThen: input =>
          validation
            .validate(input)
            .toValidated
            .as(input)
            .leftMap(_.map(Violation(_, actual = value)))
            .leftMap(Violations.apply)
    case Primitive.Number.Int(validation) =>
      value.toIntOption
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "int"), actual = value))
        .leftMap(Violations.apply)
        .andThen: input =>
          validation
            .validate(input)
            .toValidated
            .as(input)
            .leftMap(_.map(Violation(_, actual = value)))
            .leftMap(Violations.apply)
    case Primitive.Number.Long(validation) =>
      value.toLongOption
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "long"), actual = value))
        .leftMap(Violations.apply)
        .andThen: input =>
          validation
            .validate(input)
            .toValidated
            .as(input)
            .leftMap(_.map(Violation(_, actual = value)))
            .leftMap(Violations.apply)
    case Primitive.Number.Modify(self, f, _)      => parse(schema = self, value).map(f)
    case Primitive.String.Modify(self, f, _)      => parse(schema = self, value).map(f)
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
        .validate(input = value)
        .toValidated
        .as(value)
        .leftMap(_.map(Violation(_, actual = value)))
        .leftMap(Violations.apply)
