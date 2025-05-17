// package io.taig.otter.http

// import cats.data.Validated
// import cats.syntax.all.*
// import io.taig.otter.*

// final class HttpParameterParser(explode: Boolean, style: Header.Style):
//   def apply[A](name: String, codec: Http.Parameter[A], value: String): Validated[Violations, A] = codec match
//     case codec: Http.Parameter.Array[A]  => apply(name, codec, value)
//     case codec: Http.Parameter.Object[A] => apply(name, codec, value)
//     case codec: Http.Parameter.Value[A]  => apply(name, codec, value)

//   def apply[A](name: String, codec: Http.Parameter.Array[A], value: String): Validated[Violations, A] = (explode, style)
//     .match
//       case (_, Header.Style.Simple) =>
//         HttpParameterParser.parser.array.simple(value).toValidatedViolations(tpe = "array", value)
//       case (false, Header.Style.Label) =>
//         HttpParameterParser.parser.array.label.unexploded(value).toValidatedViolations(tpe = "array", value)
//       case (true, Header.Style.Label) =>
//         HttpParameterParser.parser.array.label.exploded(value).toValidatedViolations(tpe = "array", value)
//       case (false, Header.Style.Matrix) =>
//         HttpParameterParser.parser.array.matrix
//           .unexploded(value)
//           .toValidatedViolations(tpe = "array", value)
//           .andThen: (key, values) =>
//             if key === name
//             then values.valid
//             else Violations.rootNec(Violation.equal(name, actual = key)).invalid
//       case (true, Header.Style.Matrix) =>
//         HttpParameterParser.parser.array.matrix
//           .exploded(value)
//           .toValidatedViolations(tpe = "array", value)
//           .andThen: values =>
//             values.traverse: (key, value) =>
//               if key === name
//               then value.valid
//               else Violations.rootNec(Violation.equal(name, actual = key)).invalid
//     .andThen(apply(codec, _))

//   def apply[A](codec: Http.Parameter.Array[A], values: List[String]): Validated[Violations, A] = codec match
//     case Http.Parameter.Array.Collection(self) =>
//       CollectionParser(parser = HttpParameterValueParser)(codec = self, values)
//     case Http.Parameter.Array.Tuple(self) => TupleParser(parser = HttpParameterValueParser)(codec = self, values)

//   def apply[A](name: String, codec: Http.Parameter.Object[A], value: String): Validated[Violations, A] =
//     (explode, style)
//       .match
//         case (false, Header.Style.Simple) =>
//           HttpParameterParser.parser.obj.simple.unexploded(value).toValidatedViolations(tpe = "object", value)
//         case (true, Header.Style.Simple) =>
//           HttpParameterParser.parser.obj.simple.exploded(value).toValidatedViolations(tpe = "object", value)
//         case (false, Header.Style.Label) =>
//           HttpParameterParser.parser.obj.label.unexploded(value).toValidatedViolations(tpe = "object", value)
//         case (true, Header.Style.Label) =>
//           HttpParameterParser.parser.obj.label.exploded(value).toValidatedViolations(tpe = "object", value)
//         case (false, Header.Style.Matrix) =>
//           HttpParameterParser.parser.obj.matrix
//             .unexploded(value)
//             .toValidatedViolations(tpe = "object", value)
//             .andThen: (key, values) =>
//               if key === name
//               then values.valid
//               else Violations.rootNec(Violation.equal(name, actual = key)).invalid
//         case (true, Header.Style.Matrix) =>
//           HttpParameterParser.parser.obj.label.unexploded(value).toValidatedViolations(tpe = "object", value)
//       .andThen:
//         values =>
//           codec match
//             case Http.Parameter.Object.Dictionary(self) =>
//               DictionaryParser(parser = HttpParameterValueParser)(codec = self, values)
//             case Http.Parameter.Object.Record(self) => ???
//             // RecordParser(parser = HttpParameterValueParser, printer = HttpParameterValuePrinter)(codec = self, values)
//             //   .map((_, a) => a)

//   def apply[A](name: String, codec: Http.Parameter.Value[A], value: String): Validated[Violations, A] = style
//     .match
//       case Header.Style.Simple => value.valid
//       case Header.Style.Label =>
//         if value.startsWith(".")
//         then value.drop(1).valid
//         else Violations.rootNec(Violation.tpe(name = "value", actual = value)).invalid
//       case Header.Style.Matrix =>
//         HttpParameterParser.parser
//           .value(value)
//           .toValidatedViolations(tpe = "value", value)
//           .andThen: (key, value) =>
//             if key === name
//             then value.valid
//             else Violations.rootNec(Violation.equal(reference = name, actual = key)).invalid
//     .andThen(HttpParameterValueParser(codec, _))

// object HttpParameterParser:
//   private object parser:
//     import cats.parse.Parser
//     import cats.parse.Parser.*

//     def token(escape: Char*): Parser[Char] =
//       charWhere(value => value != '\\' && !escape.contains(value)).orElse(char('\\') *> anyChar)

//     object array:
//       val simple: String => Either[Error, List[String]] =
//         val parser = token(',').rep.string.repSep0(char(','))
//         (value: String) => parser.parseAll(value).map(_.map(unescape(_, ",")))

//       object label:
//         val unexploded: String => Either[Error, List[String]] = simple

//         val exploded: String => Either[Error, List[String]] =
//           val parser = token('.').rep.string.repSep0(char('.'))
//           (value: String) => parser.parseAll(value).map(_.map(unescape(_, ".")))

//       object matrix:
//         val unexploded: String => Either[Error, (String, List[String])] =
//           val parser = (char(';') *> token('=').rep.string <* char('=')) ~ token(',').rep.string.repSep0(char(','))
//           (value: String) => parser.parseAll(value).map(_.bimap(unescape(_, "="), _.map(unescape(_, ","))))

//         val exploded: String => Either[Error, List[(String, String)]] =
//           val parser =
//             char(';') *> ((token(';', '=').rep.string <* char('=')) ~ token(',').rep.string).repSep0(char(';'))
//           (value: String) => parser.parseAll(value).map(_.map(_.bimap(unescape(_, List(";", "=")), unescape(_, ";"))))

//     object obj:
//       object simple:
//         val unexploded: String => Either[Error, List[(String, String)]] =
//           val parser = ((token(',').rep.string <* char(',')) ~ token(',').rep.string).repSep0(char(','))
//           (value: String) => parser.parseAll(value).map(_.map(_.bimap(unescape(_, ","), unescape(_, ","))))

//         val exploded: String => Either[Error, List[(String, String)]] =
//           val parser = ((token(',', '=').rep.string <* char('=')) ~ token(',').rep.string).repSep0(char(','))
//           (value: String) => parser.parseAll(value).map(_.map(_.bimap(unescape(_, List(",", "=")), unescape(_, ","))))

//       object label:
//         val unexploded: String => Either[Error, List[(String, String)]] =
//           val parser = ((char('.') *> token(',').rep.string <* char(',')) ~ token(',').rep.string).repSep0(char(','))
//           (value: String) => parser.parseAll(value).map(_.map(_.bimap(unescape(_, ","), unescape(_, ","))))

//         val exploded: String => Either[Error, List[(String, String)]] =
//           val parser =
//             ((char('.') *> token('.', '=').rep.string <* char('=')) ~ token('.').rep.string).repSep0(char('.'))
//           (value: String) => parser.parseAll(value).map(_.map(_.bimap(unescape(_, List(".", "=")), unescape(_, "."))))

//       object matrix:
//         val unexploded: String => Either[Error, (String, List[(String, String)])] =
//           val parser =
//             (char(';') *> token('=').rep.string <* char('=')) ~ ((token(',').rep.string <* char(',')) ~ token(
//               ','
//             ).rep.string).repSep0(char(','))
//           (value: String) =>
//             parser.parseAll(value).map(_.bimap(unescape(_, "="), _.map(_.bimap(unescape(_, ","), unescape(_, ",")))))

//         val exploded: String => Either[Error, List[(String, String)]] =
//           val parser = char(';') *> ((token('=').rep.string <* char('=')) ~ (token(';').rep.string)).repSep0(char(';'))
//           (value: String) => parser.parseAll(value).map(_.map(_.bimap(unescape(_, "="), unescape(_, ";"))))

//     val value: String => Either[Error, (String, String)] =
//       val parser = (char(';') *> token('=').rep0.string <* char('=')) ~ anyChar.rep0.string
//       (value: String) => parser.parseAll(value).map(_.leftMap(unescape(_, "=")))
