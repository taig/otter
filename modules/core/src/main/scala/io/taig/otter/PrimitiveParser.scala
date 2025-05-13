// package io.taig.otter

// import cats.data.Validated
// import cats.syntax.all.*

// import java.math.BigDecimal as JBigDecimal
// import java.math.BigInteger as JBigInteger

// final class PrimitiveParser(quotes: Boolean) extends Parser[Primitive]:
//   override def apply[A](codec: Primitive[A], value: String): Validated[Violations, A] = codec match
//     case Primitive.Boolean.Modify(self, f, _) => apply(codec = self, value).map(f)
//     case Primitive.Boolean.Root(_) =>
//       value.toBooleanOption.toValid(Violations.rootNec(Violation.tpe(name = "long", actual = value)))
//     case Primitive.Number.BigDecimal(_, _, _, _) =>
//       Validated
//         .catchOnly[NumberFormatException](JBigDecimal(value))
//         .leftMap: exception =>
//           Violations.rootNec(Violation.tpe(name = "bigDecimal", actual = value, hint = Option(exception.getMessage())))
//     case Primitive.Number.BigInteger(_, _, _, _) =>
//       Validated
//         .catchOnly[NumberFormatException](JBigInteger(value))
//         .leftMap: exception =>
//           Violations.rootNec(Violation.tpe(name = "bigInteger", actual = value, hint = Option(exception.getMessage())))
//     case Primitive.Number.Double(_, _, _, _) =>
//       value.toDoubleOption.toValid(Violations.rootNec(Violation.tpe(name = "double", actual = value)))
//     case Primitive.Number.Float(_, _, _, _) =>
//       value.toFloatOption.toValid(Violations.rootNec(Violation.tpe(name = "float", actual = value)))
//     case Primitive.Number.Int(_, _, _, _) =>
//       value.toIntOption.toValid(Violations.rootNec(Violation.tpe(name = "int", actual = value)))
//     case Primitive.Number.Long(_, _, _, _) =>
//       value.toLongOption.toValid(Violations.rootNec(Violation.tpe(name = "long", actual = value)))
//     case Primitive.Number.Modify(self, f, _) => apply(codec = self, value).map(f)
//     case Primitive.String.Modify(self, f, _) => apply(codec = self, value).map(f)
//     case Primitive.String.Parser(_, decode, _, _, _, _, _) =>
//       val input =
//         if quotes then
//           Parsers.text
//             .parseAll(value)
//             .toValidated
//             .leftMap(_ => Violations.rootNec(Violation.tpe(name = "string", actual = value)))
//         else value.valid

//       input.andThen(decode(_).toValidated.leftMap: error =>
//         Violations.rootNec(Violation.tpe(name = "string", actual = value, hint = error)))
//     case Primitive.String.Text(_, _, _, _) =>
//       if quotes then
//         Parsers.text
//           .parseAll(value)
//           .toValidated
//           .leftMap(_ => Violations.rootNec(Violation.tpe(name = "string", actual = value)))
//       else value.valid

// object PrimitiveParser:
//   val Quoted: Parser[Primitive] = PrimitiveParser(quotes = true)
//   val Unquoted: Parser[Primitive] = PrimitiveParser(quotes = false)
