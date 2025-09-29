package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Parsers
import io.taig.otter.Primitive
import io.taig.otter.Violations
import io.taig.otter.validation.Violation

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

final class PrimitiveParser[S[_]](parser: Decoder[S, String])(quotes: Boolean) extends Decoder[Primitive[S, *], String]:
  override def decode[A](schema: Primitive[S, A], value: String): Validated[Violations, A] =
    decode(schema = schema.value, value)

  def decode[A](schema: Primitive.Value[S, A], value: String): Validated[Violations, A] = schema match
    case Primitive.Value.Boolean.Modify(self, f, _) => decode(schema = self, value).map(f)
    case Primitive.Value.Boolean.Root               =>
      value.toBooleanOption.toValid(Violations.rootNec(Violation.tpe(name = "long", actual = value)))
    case Primitive.Value.Number.BigDecimal(_, _, _) =>
      Validated
        .catchOnly[NumberFormatException](JBigDecimal(value))
        .leftMap: exception =>
          Violations.rootNec(Violation.tpe(name = "bigDecimal", actual = value, hint = Option(exception.getMessage())))
    case Primitive.Value.Number.BigInteger(_, _, _) =>
      Validated
        .catchOnly[NumberFormatException](JBigInteger(value))
        .leftMap: exception =>
          Violations.rootNec(Violation.tpe(name = "bigInteger", actual = value, hint = Option(exception.getMessage())))
    case Primitive.Value.Number.Double(_, _, _) =>
      value.toDoubleOption.toValid(Violations.rootNec(Violation.tpe(name = "double", actual = value)))
    case Primitive.Value.Number.Float(_, _, _) =>
      value.toFloatOption.toValid(Violations.rootNec(Violation.tpe(name = "float", actual = value)))
    case Primitive.Value.Number.Int(_, _, _) =>
      value.toIntOption.toValid(Violations.rootNec(Violation.tpe(name = "int", actual = value)))
    case Primitive.Value.Number.Long(_, _, _) =>
      value.toLongOption.toValid(Violations.rootNec(Violation.tpe(name = "long", actual = value)))
    case Primitive.Value.Number.Modify(self, f, _)   => decode(schema = self, value).map(f)
    case Primitive.Value.String.Modify(self, f, _)   => decode(schema = self, value).map(f)
    case Primitive.Value.String.Parsed(self)         => parser.decode(schema = self.value, value)
    case Primitive.Value.String.Parser(_, decode, _) =>
      val input =
        if quotes then
          Parsers.text
            .parseAll(value)
            .toValidated
            .leftMap(_ => Violations.rootNec(Violation.tpe(name = "string", actual = value)))
        else value.valid

      input.andThen(decode(_).toValidated.leftMap: error =>
        Violations.rootNec(Violation.tpe(name = "string", actual = value, hint = error)))
    case Primitive.Value.String.Text(_, _, _) =>
      if quotes then
        Parsers.text
          .parseAll(value)
          .toValidated
          .leftMap(_ => Violations.rootNec(Violation.tpe(name = "string", actual = value)))
      else value.valid
