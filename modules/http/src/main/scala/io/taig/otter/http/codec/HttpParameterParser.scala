package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.*
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Http
import io.taig.otter.http.Parameter

final class HttpParameterParser(name: String, style: Parameter.Style) extends Decoder[Http.Parameter, String]:
  override def decode[A](schema: Http.Parameter[A], value: String): Validated[Violations, A] = schema match
    case schema: Http.Parameter.Value[A] => HttpParameterValueParser.decode(schema, value)
    case schema: Http.Parameter.Array[A] =>
      style
        .match
          case Parameter.Style.Simple => parser.array.simple(value)
          case Parameter.Style.Label  => parser.array.label(value)
          case Parameter.Style.Matrix => parser.array.matrix(value).map(_.collect { case (`name`, value) => value })
        .leftMap(error =>
          Violations.rootNec(Violation.tpe(name = "parameter.array", actual = value, hint = error.show))
        )
        .toValidated
        .andThen(values => HttpParameterArrayDecoder.decode(schema, Chain.fromSeq(values)))
    case schema: Http.Parameter.Object[A] =>
      style
        .match
          case Parameter.Style.Simple => parser.obj.simple(value)
          case Parameter.Style.Label  => parser.obj.label(value)
          case Parameter.Style.Matrix => parser.obj.matrix(value)
        .leftMap(error =>
          Violations.rootNec(Violation.tpe(name = "parameter.object", actual = value, hint = error.show))
        )
        .toValidated
        .andThen(values => HttpParameterObjectDecoder.decode(schema, Chain.fromSeq(values)))

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
