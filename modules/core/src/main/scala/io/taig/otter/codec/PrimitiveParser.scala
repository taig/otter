package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Parsers
import io.taig.otter.Primitive
import io.taig.otter.Violation
import io.taig.otter.Violations

import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger

final class PrimitiveParser(quotes: Boolean) extends Decoder[Primitive, String]:
  override def decode[A](schema: Primitive[A], value: String): Validated[Violations, A] = schema match
    case Primitive.Boolean.Modify(self, f, _) => decode(schema = self, value).map(f)
    case Primitive.Boolean.Root =>
      value.toBooleanOption.toValid(Violations.rootNec(Violation.tpe(name = "long", actual = value)))
    case Primitive.Number.BigDecimal(_, _, _) =>
      Validated
        .catchOnly[NumberFormatException](JBigDecimal(value))
        .leftMap: exception =>
          Violations.rootNec(Violation.tpe(name = "bigDecimal", actual = value, hint = Option(exception.getMessage())))
    case Primitive.Number.BigInteger(_, _, _) =>
      Validated
        .catchOnly[NumberFormatException](JBigInteger(value))
        .leftMap: exception =>
          Violations.rootNec(Violation.tpe(name = "bigInteger", actual = value, hint = Option(exception.getMessage())))
    case Primitive.Number.Double(_, _, _) =>
      value.toDoubleOption.toValid(Violations.rootNec(Violation.tpe(name = "double", actual = value)))
    case Primitive.Number.Float(_, _, _) =>
      value.toFloatOption.toValid(Violations.rootNec(Violation.tpe(name = "float", actual = value)))
    case Primitive.Number.Int(_, _, _) =>
      value.toIntOption.toValid(Violations.rootNec(Violation.tpe(name = "int", actual = value)))
    case Primitive.Number.Long(_, _, _) =>
      value.toLongOption.toValid(Violations.rootNec(Violation.tpe(name = "long", actual = value)))
    case Primitive.Number.Modify(self, f, _) => decode(schema = self, value).map(f)
    case Primitive.String.Modify(self, f, _) => decode(schema = self, value).map(f)
    case Primitive.String.Parser(_, decode, _, _, _, _) =>
      val input =
        if quotes then
          Parsers.text
            .parseAll(value)
            .toValidated
            .leftMap(_ => Violations.rootNec(Violation.tpe(name = "string", actual = value)))
        else value.valid

      input.andThen(decode(_).toValidated.leftMap: error =>
        Violations.rootNec(Violation.tpe(name = "string", actual = value, hint = error)))
    case Primitive.String.Text(_, _, _) =>
      if quotes then
        Parsers.text
          .parseAll(value)
          .toValidated
          .leftMap(_ => Violations.rootNec(Violation.tpe(name = "string", actual = value)))
      else value.valid

object PrimitiveParser:
  val Quoted: Decoder[Primitive, String] = PrimitiveDecoder(PrimitiveParser(quotes = true))
  val Unquoted: Decoder[Primitive, String] = PrimitiveDecoder(PrimitiveParser(quotes = false))
