package io.taig.otter.http

import cats.parse.Numbers.digit
import cats.parse.Parser
import cats.parse.Parser0
import cats.parse.strings.Json
import cats.syntax.all.*
import io.taig.otter.http.header.Accept
import io.taig.otter.http.header.MediaRange
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.header.Parameter
import io.taig.otter.http.header.Parameters
import io.taig.otter.http.header.Weighted
import org.typelevel.ci.*

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
    (value >= '0' && value <= '9') ||
    value == '.' || value == '-' || value == '_'
  }

  val string: Parser[String] = Json.delimited.parser

  val parameter: Parser[Parameter] = ((token.map(CIString.apply) <* equal) ~ (string | token)).map(Parameter.apply)

  val parameters: Parser0[Parameters] =
    (whitespace.with1 *> semicolon *> whitespace *> parameter).rep0.map(Parameters.apply)

  val mediaType: Parser[MediaType] =
    val tpe = ((token <* slash) ~ token).map(MediaType.Type.apply)
    (tpe ~ parameters).map(MediaType.apply)
    // (tpe <* Parser.anyChar.rep0).map(MediaType(_, Parameters.Empty))

  val mediaRange: Parser[MediaRange] =
    val tpe: Parser[MediaRange.Type] =
      (star *> slash *> star).as(MediaRange.Type.Any).backtrack |
        (token <* slash <* star).map(MediaRange.Type.Primary.apply).backtrack |
        ((token <* slash) ~ token).map(MediaRange.Type.Secondary.apply)

    (tpe ~ parameters).map(MediaRange.apply)

  object q:
    val value: Parser[BigDecimal] =
      val zero = (Parser.char('0') *> (dot *> digit.rep0(min = 0, max = 3)).?).map:
        case Some(digits) => BigDecimal(s"0.${digits.mkString}")
        case None         => BigDecimal(0)

      val one = (Parser.char('1') *> (dot *> Parser.char('0').rep0(min = 0, max = 3)).?).as(BigDecimal(1))

      zero | one

    val parameter: Parser[BigDecimal] = Parser.ignoreCaseChar('q') *> equal *> value

  val weightedMediaRange: Parser[Weighted[MediaRange]] = mediaRange.map: mediaRange =>
    val qValueWithIndex = mediaRange.parameters.toList.zipWithIndex
      .collect { case (parameter, index) if parameter.name === ci"q" => (index, parameter.value) }
      .reverse
      .collectFirstSome { case (index, value) =>
        q.value.parseAll(value).toOption.tupleRight(index)
      }

    val qValue = qValueWithIndex.map { case (qValue, _) => qValue }

    val parametersWithoutQValue = qValueWithIndex.fold(mediaRange.parameters) { case (_, index) =>
      Parameters(mediaRange.parameters.toList.patch(index, Nil, 1))
    }

    Weighted(mediaRange.copy(parameters = parametersWithoutQValue), qValue)

  val accept: Parser[Accept] = weightedMediaRange.repSep(separator).map(Accept.apply)

  val error: Parser[String] = Parser.string("Error:") *> whitespace *> token
