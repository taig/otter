package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Constraint
import io.taig.otter.Parsers
import io.taig.otter.Primitive
import io.taig.otter.Violation
import io.taig.otter.Violations

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

final class PrimitiveParser[S[_]](parser: Decoder[S, String])(quotes: Boolean) extends Decoder[Primitive[S, *], String]:
  override def decode[A](schema: Primitive[S, A], value: String): Validated[Violations, A] =
    decode(schema = schema.value, value)

  def decode[A](schema: Primitive.Value[S, A], value: String): Validated[Violations, A] = schema match
    case Primitive.Value.Boolean.Modify(self, f, _) => decode(schema = self, value).map(f)
    case Primitive.Value.Boolean.Root               =>
      value.toBooleanOption.toValid(
        Violations.rootNec(
          Violation.fromConstraint(constraint = Constraint.Generic.Type(name = "boolean"), actual = value)
        )
      )
    case Primitive.Value.Number.BigDecimal(_) =>
      Validated
        .catchOnly[NumberFormatException](JBigDecimal(value))
        .leftMap: exception =>
          val violation = Violation.fromConstraint(
            constraint = Constraint.Generic.Type(name = "bigDecimal"),
            actual = value,
            hint = exception.getMessage.some
          )
          Violations.rootNec(violation)
    case Primitive.Value.Number.BigInteger(_) =>
      Validated
        .catchOnly[NumberFormatException](JBigInteger(value))
        .leftMap: exception =>
          val violation = Violation.fromConstraint(
            constraint = Constraint.Generic.Type(name = "bigInteger"),
            actual = value,
            hint = exception.getMessage.some
          )
          Violations.rootNec(violation)
    case Primitive.Value.Number.Double(_) =>
      value.toDoubleOption.toValid(
        Violations.rootNec(
          Violation.fromConstraint(constraint = Constraint.Generic.Type(name = "double"), actual = value)
        )
      )
    case Primitive.Value.Number.Float(_) =>
      value.toFloatOption.toValid(
        Violations.rootNec(
          Violation.fromConstraint(constraint = Constraint.Generic.Type(name = "float"), actual = value)
        )
      )
    case Primitive.Value.Number.Int(_) =>
      value.toIntOption.toValid(
        Violations.rootNec(Violation.fromConstraint(constraint = Constraint.Generic.Type(name = "int"), actual = value))
      )
    case Primitive.Value.Number.Long(_) =>
      value.toLongOption.toValid(
        Violations.rootNec(
          Violation.fromConstraint(constraint = Constraint.Generic.Type(name = "long"), actual = value)
        )
      )
    case Primitive.Value.Number.Modify(self, f, _)   => decode(schema = self, value).map(f)
    case Primitive.Value.String.Modify(self, f, _)   => decode(schema = self, value).map(f)
    case Primitive.Value.String.Parsed(self)         => parser.decode(schema = self.value, value)
    case Primitive.Value.String.Parser(_, decode, _) =>
      val input =
        if quotes then
          Parsers.text
            .parseAll(value)
            .toValidated
            .leftMap: _ =>
              val violation =
                Violation.fromConstraint(constraint = Constraint.Generic.Type(name = "string"), actual = value)
              Violations.rootNec(violation)
        else value.valid

      input.andThen: value =>
        decode(value).toValidated.leftMap: error =>
          val violation = Violation.fromConstraint(
            constraint = Constraint.Generic.Type(name = "string"),
            actual = value,
            hint = error.some
          )
          Violations.rootNec(violation)

    case Primitive.Value.String.Text(_) =>
      if quotes then
        Parsers.text
          .parseAll(value)
          .toValidated
          .leftMap: _ =>
            val violation =
              Violation.fromConstraint(constraint = Constraint.Generic.Type(name = "string"), actual = value)
            Violations.rootNec(violation)
      else value.valid
