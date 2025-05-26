package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.*
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Parameter

final class ParameterValueParser(name: String, style: Parameter.Style) extends Decoder[Parameter.Schema, String]:
  override def decode[A](schema: Parameter.Schema[A], value: String): Validated[Violations, A] = schema match
    case schema: Parameter.Schema.Atom[A] => ParameterValueAtomParser.decode(schema, value)
    case schema: Parameter.Schema.Array[A] =>
      style
        .match
          case Parameter.Style.Simple => parser.array.simple(value)
          case Parameter.Style.Label  => parser.array.label(value)
          case Parameter.Style.Matrix => parser.array.matrix(value).map(_.collect { case (`name`, value) => value })
        .leftMap(error =>
          Violations.rootNec(Violation.tpe(name = "parameter.array", actual = value, hint = error.show))
        )
        .toValidated
        .andThen(values => ParameterValueArrayDecoder.decode(schema, Chain.fromSeq(values)))
    case schema: Parameter.Schema.Object[A] =>
      style
        .match
          case Parameter.Style.Simple => parser.obj.simple(value)
          case Parameter.Style.Label  => parser.obj.label(value)
          case Parameter.Style.Matrix => parser.obj.matrix(value)
        .leftMap(error =>
          Violations.rootNec(Violation.tpe(name = "parameter.object", actual = value, hint = error.show))
        )
        .toValidated
        .andThen(values => ParameterValueObjectDecoder.decode(schema, Chain.fromSeq(values)))

  private object parser:
    import cats.parse.Parser
    import cats.parse.Parser.*

    def token(escape: Char*): Parser[String] =
      charWhere(value => value != '\\' && !escape.contains(value)).orElse(char('\\') *> anyChar).rep.string

    object array:
      val simple: String => Either[Error, List[String]] =
        val parser = token(',').repSep0(char(','))
        (value: String) => parser.parseAll(value).map(_.map(unescape(_, ",")))

      val label: String => Either[Error, List[String]] =
        val parser = token('.').repSep0(char('.'))
        (value: String) => parser.parseAll(value).map(_.map(unescape(_, ".")))

      val matrix: String => Either[Error, List[(String, String)]] =
        val parser = char(';') *> ((token(';', '=') <* char('=')) ~ token(',')).repSep0(char(';'))
        (value: String) => parser.parseAll(value).map(_.map(_.bimap(unescape(_, List(";", "=")), unescape(_, ";"))))

    object obj:
      val simple: String => Either[Error, List[(String, Option[String])]] =
        val parser = (token(',', '=') ~ (char('=') *> token(',')).?).repSep0(char(','))
        (value: String) =>
          parser.parseAll(value).map(_.map(_.bimap(unescape(_, List(",", "=")), _.map(unescape(_, ",")))))

      val label: String => Either[Error, List[(String, Option[String])]] =
        val parser = (char('.') *> token('.', '=') ~ (char('=') *> token('.')).?).repSep0(char('.'))
        (value: String) =>
          parser.parseAll(value).map(_.map(_.bimap(unescape(_, List(".", "=")), _.map(unescape(_, ".")))))

      val matrix: String => Either[Error, List[(String, Option[String])]] =
        val parser = char(';') *> (token('=') ~ (char('=') *> token(';')).?).repSep0(char(';'))
        (value: String) => parser.parseAll(value).map(_.map(_.bimap(unescape(_, "="), _.map(unescape(_, ";")))))
