package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Primitive
import io.taig.otter.Constraint
import io.taig.otter.Violations
import cats.syntax.all.*
import io.taig.validation.Violation
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

object PrimitiveParser extends Parser[Primitive.Read]:
  override def decode[A](schema: Primitive.Read[A], value: String): Validated[Violations, A] = schema match
    case Primitive.Boolean.Modify(self, f, _)   => decode(schema = self, value).map(f)
    case Primitive.Boolean.Read.Modify(self, f) => decode(schema = self, value).map(f)
    case Primitive.Boolean.Root                 =>
      value.toBooleanOption
        .toValid(Violation(constraint = Constraint.Generic.Type(name = "boolean"), actual = value, hint = none))
        .leftMap(Violations.apply)
    case Primitive.Modify(self, f, _)            => decode(schema = self, value).map(f)
    case Primitive.Number.BigDecimal(validation) =>
      Validated
        .catchOnly[NumberFormatException](new JBigDecimal(value))
        .leftMap(_ => Constraint.Generic.Type(name = "bigDecimal"))
        .leftMap(Violation(_, actual = value, hint = none))
        .leftMap(Violations.apply)
        .andThen(value => validation.validate(value).toInvalid(value).leftMap(Violations.apply))
    case Primitive.Number.BigInteger(validation) =>
      Validated
        .catchOnly[NumberFormatException](new JBigInteger(value))
        .leftMap(_ => Constraint.Generic.Type(name = "bigInteger"))
        .leftMap(Violation(_, actual = value, hint = none))
        .leftMap(Violations.apply)
        .andThen(value => validation.validate(value).toInvalid(value).leftMap(Violations.apply))
    case Primitive.Number.Double(validation) =>
      value.toDoubleOption
        .toValid(Constraint.Generic.Type(name = "double"))
        .leftMap(Violation(_, actual = value, hint = none))
        .leftMap(Violations.apply)
        .andThen(value => validation.validate(value).toInvalid(value).leftMap(Violations.apply))
    case Primitive.Number.Float(validation) =>
      value.toFloatOption
        .toValid(Constraint.Generic.Type(name = "float"))
        .leftMap(Violation(_, actual = value, hint = none))
        .leftMap(Violations.apply)
        .andThen(value => validation.validate(value).toInvalid(value).leftMap(Violations.apply))
    case Primitive.Number.Int(validation) =>
      value.toIntOption
        .toValid(Constraint.Generic.Type(name = "int"))
        .leftMap(Violation(_, actual = value, hint = none))
        .leftMap(Violations.apply)
        .andThen(value => validation.validate(value).toInvalid(value).leftMap(Violations.apply))
    case Primitive.Number.Long(validation) =>
      value.toLongOption
        .toValid(Constraint.Generic.Type(name = "long"))
        .leftMap(Violation(_, actual = value, hint = none))
        .leftMap(Violations.apply)
        .andThen(value => validation.validate(value).toInvalid(value).leftMap(Violations.apply))
    case Primitive.Number.Modify(self, f, _)   => decode(schema = self, value).map(f)
    case Primitive.Number.Read.Modify(self, f) => decode(schema = self, value).map(f)
    case Primitive.Text.Codec(name, parse, _)  =>
      parse(value).toValidated
        .leftMap: error =>
          Violation(constraint = Constraint.Generic.Type(name), actual = value, hint = error.some)
        .leftMap(Violations.apply)
    case Primitive.Text.Modify(self, f, _)       => decode(schema = self, value).map(f)
    case Primitive.Text.Read.Modify(self, f)     => decode(schema = self, value).map(f)
    case Primitive.Text.Read.Parser(name, parse) =>
      parse(value).toValidated
        .leftMap: error =>
          Violation(constraint = Constraint.Generic.Type(name), actual = value, hint = error.some)
        .leftMap(Violations.apply)
    case Primitive.Text.Root(validation) =>
      validation.validate(value).toInvalid(value).leftMap(Violations.apply)
