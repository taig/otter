package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.*

final class HttpSegmentParser(explode: Boolean, style: Header.Style):
  def apply[A](name: String, codec: Http.Segment[A], value: String): Validated[Violations, A] = codec match
    case codec: Http.Segment.Array[A]  => apply(name, codec, value)
    case codec: Http.Segment.Object[A] => apply(name, codec, value)
    case codec: Http.Segment.Value[A]  => apply(name, codec, value)

  def apply[A](name: String, codec: Http.Segment.Array[A], value: String): Validated[Violations, A] = (explode, style)
    .match
      case (_, Header.Style.Simple) =>
        HttpSegmentParser.parser.array.simple(value).toValidatedViolations(tpe = "array", value)
      case (false, Header.Style.Label) =>
        HttpSegmentParser.parser.array.label.unexploded(value).toValidatedViolations(tpe = "array", value)
      case (true, Header.Style.Label) =>
        HttpSegmentParser.parser.array.label.exploded(value).toValidatedViolations(tpe = "array", value)
      case (false, Header.Style.Matrix) =>
        HttpSegmentParser.parser.array.matrix
          .unexploded(value)
          .toValidatedViolations(tpe = "array", value)
          .andThen: (key, values) =>
            if key === name
            then values.valid
            else Violations.rootNec(Violation.equal(name, actual = key)).invalid
      case (true, Header.Style.Matrix) =>
        HttpSegmentParser.parser.array.matrix
          .exploded(value)
          .toValidatedViolations(tpe = "array", value)
          .andThen: values =>
            values.traverse: (key, value) =>
              if key === name
              then value.valid
              else Violations.rootNec(Violation.equal(name, actual = key)).invalid
    .andThen(apply(codec, _))

  def apply[A](codec: Http.Segment.Array[A], values: List[String]): Validated[Violations, A] = codec match
    case Http.Segment.Array.Collection(self) => CollectionParser(parser = HttpSegmentValueParser)(codec = self, values)
    case Http.Segment.Array.Tuple(self)      => TupleParser(parser = HttpSegmentValueParser)(codec = self, values)

  def apply[A](name: String, codec: Http.Segment.Object[A], value: String): Validated[Violations, A] = ???

  def apply[A](name: String, codec: Http.Segment.Value[A], value: String): Validated[Violations, A] = style
    .match
      case Header.Style.Simple => value.valid
      case Header.Style.Label =>
        if value.startsWith(".")
        then value.drop(1).valid
        else Violations.rootNec(Violation.tpe(name = "value", actual = value)).invalid
      case Header.Style.Matrix =>
        HttpSegmentParser.parser
          .value(value)
          .toValidatedViolations(tpe = "value", value)
          .andThen: (key, value) =>
            if key === name
            then value.valid
            else Violations.rootNec(Violation.equal(reference = name, actual = key)).invalid
    .andThen(HttpSegmentValueParser(codec, _))

object HttpSegmentParser:
  private object parser:
    import cats.parse.Parser
    import cats.parse.Parser.*

    def token(escape: Char*): Parser[Char] =
      charWhere(value => value != '\\' && !escape.contains(value)).orElse(char('\\') *> anyChar)

    object array:
      val simple: String => Either[Error, List[String]] =
        val parser = token(',').rep.string.repSep0(char(','))
        (value: String) => parser.parseAll(value).map(_.map(unescape(_, ",")))

      object label:
        val unexploded: String => Either[Error, List[String]] = simple

        val exploded: String => Either[Error, List[String]] =
          val parser = token('.').rep.string.repSep0(char('.'))
          (value: String) => parser.parseAll(value).map(_.map(unescape(_, ".")))

      object matrix:
        val unexploded: String => Either[Error, (String, List[String])] =
          val parser = (char(';') *> token('=').rep.string <* char('=')) ~ token(',').rep.string.repSep0(char(','))
          (value: String) => parser.parseAll(value).map(_.bimap(unescape(_, "="), _.map(unescape(_, ","))))

        val exploded: String => Either[Error, List[(String, String)]] =
          val parser =
            char(';') *> ((token(';', '=').rep.string <* char('=')) ~ token(',').rep.string).repSep0(char(';'))
          (value: String) => parser.parseAll(value).map(_.map(_.bimap(unescape(_, List(";", "=")), unescape(_, ";"))))

    val value: String => Either[Error, (String, String)] =
      val parser = (char(';') *> token('=').rep0.string <* char('=')) ~ anyChar.rep0.string
      (value: String) => parser.parseAll(value).map(_.leftMap(unescape(_, "=")))
