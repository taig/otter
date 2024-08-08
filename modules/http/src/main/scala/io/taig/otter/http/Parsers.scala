package io.taig.otter.http

import cats.parse.Parser
import io.taig.otter.http.header.ContentType
import io.taig.otter.http.header.Parameter
import cats.parse.Numbers.digit
import cats.parse.strings.Json
import io.taig.otter.http.header.Weighted
import cats.parse.Parser0
import io.taig.otter.http.header.Accept
import io.taig.otter.http.header.MediaRange

private[http] object Parsers:
  val whitespace: Parser0[Unit] = Parser.charIn(" \t\r\n").rep0.void

  val slash: Parser[Unit] = Parser.char('/')

  val semicolon: Parser[Unit] = Parser.char(';')

  val equal: Parser[Unit] = Parser.char('=')

  val dot: Parser[Unit] = Parser.char('.')

  val star: Parser[Unit] = Parser.char('*')

  val comma: Parser[Unit] = Parser.char(',')

  val separator: Parser[Unit] = comma.surroundedBy(whitespace)

  val token: Parser[String] = Parser.charsWhile { value =>
    (value >= 'a' && value <= 'z') ||
    (value >= 'A' && value <= 'Z') ||
    (value >= '0' && value <= '9')
  }

  val string: Parser[String] = Json.delimited.parser

  val parameter: Parser[Parameter] = ((token <* equal) ~ (string | token)).map(Parameter.apply)

  val parameters: Parser0[List[Parameter]] =
    (whitespace.with1 *> semicolon *> whitespace *> parameter).rep0

  val contentType: Parser0[ContentType] = ((token <* slash) ~ token ~ parameters)
    .map { case ((tpe, subtype), parameters) => ContentType(tpe, subtype, parameters) }

  val mediaRangeType: Parser[MediaRange.Type] =
    (star *> slash *> star).as(MediaRange.Type.Any).backtrack |
      (token <* slash <* star).map(MediaRange.Type.Primary.apply).backtrack |
      ((token <* slash) ~ token).map(MediaRange.Type.Secondary.apply)

  val mediaRange: Parser[MediaRange] = (mediaRangeType ~ parameters).map(MediaRange.apply)

  val q: Parser[BigDecimal] =
    val zero = (Parser.char('0') *> (dot *> digit.rep0(min = 0, max = 3)).?).map:
      case Some(digits) => BigDecimal(s"0.${digits.mkString}")
      case None         => BigDecimal(0)

    val one = (Parser.char('1') *> (dot *> Parser.char('0').rep0(min = 0, max = 3)).?).as(BigDecimal(1))

    val value: Parser[BigDecimal] = zero | one

    Parser.char('q') *> equal *> value

  def weighted[A](parser: Parser[BigDecimal] => Parser[(A, Option[BigDecimal])]): Parser[Weighted[A]] =
    parser(q).map(Weighted.apply)

  val weightedMediaRange: Parser[Weighted[MediaRange]] =
    (mediaRange ~ q.?).map(Weighted.apply)

  val accept: Parser[Accept] = weightedMediaRange.repSep(separator).map(Accept.apply)
