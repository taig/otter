package io.taig.otter.codec

import io.taig.otter.Constraint
import cats.data.Validated
import io.taig.otter.Primitive
import cats.syntax.all.*
import io.taig.otter.Violation
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object PrimitiveParser extends Parser[Primitive]:
  override def parse[A](schema: Primitive[A], value: String): Validated[Violation, A] = schema match
    case Primitive.Boolean.Modify(self, f, _) => parse(schema = self, value).map(f)
    case Primitive.Boolean.Root               =>
      value.toBooleanOption
        .toValid(Violation.fromConstraint(constraint = Constraint.Generic.Type(name = "boolean"), actual = value))
    case Primitive.Number.BigDecimal(_) =>
      Validated
        .catchOnly[NumberFormatException](JBigDecimal(value))
        .leftMap: exception =>
          Violation.fromConstraint(
            constraint = Constraint.Generic.Type(name = "bigDecimal"),
            actual = value,
            hint = exception.getMessage.some
          )
    case Primitive.Number.BigInteger(_) =>
      Validated
        .catchOnly[NumberFormatException](JBigInteger(value))
        .leftMap: exception =>
          Violation.fromConstraint(
            constraint = Constraint.Generic.Type(name = "bigInteger"),
            actual = value,
            hint = exception.getMessage.some
          )
    case Primitive.Number.Double(_) =>
      value.toDoubleOption
        .toValid(Violation.fromConstraint(constraint = Constraint.Generic.Type(name = "double"), actual = value))
    case Primitive.Number.Float(_) =>
      value.toFloatOption
        .toValid(Violation.fromConstraint(constraint = Constraint.Generic.Type(name = "float"), actual = value))
    case Primitive.Number.Int(_) =>
      value.toIntOption
        .toValid(Violation.fromConstraint(constraint = Constraint.Generic.Type(name = "int"), actual = value))
    case Primitive.Number.Long(_) =>
      value.toLongOption
        .toValid(Violation.fromConstraint(constraint = Constraint.Generic.Type(name = "long"), actual = value))
    case Primitive.Number.Modify(self, f, _)      => parse(schema = self, value).map(f)
    case Primitive.String.Modify(self, f, _)      => parse(schema = self, value).map(f)
    case Primitive.String.Parser(name, decode, _) =>
      decode(value).toValidated.leftMap: error =>
        Violation.fromConstraint(
          constraint = Constraint.Generic.Type(name),
          actual = value,
          hint = error.some
        )
    case Primitive.String.Root(validation) =>
      validation
        .validate(input = value)
        .toValidated
        .as(value)
        .leftMap(Violation.fromConstraints(_, actual = value))
