package io.taig.otter.http

import cats.parse.Parser
import io.taig.otter.http.header.ContentType
import cats.parse.Parser0
import cats.parse.strings.Json

private[http] object Parsers:
  val whitespace: Parser0[Unit] = Parser.charIn(" \t\r\n").rep0.void

  val slash: Parser[Unit] = Parser.char('/')

  val semicolon: Parser[Unit] = Parser.char(';')

  val equal: Parser[Unit] = Parser.char('=')

  val token: Parser0[String] =
    val forbidden = Set('=', '/', ';', '*', '"')
    Parser.charsWhile(!forbidden.contains(_))

  val string: Parser[String] = Json.delimited.parser

  object contentType:
    val parameter: Parser[ContentType.Parameter] =
      ((token.with1 <* equal) ~ (string | token)).map(ContentType.Parameter.apply)

    val parameters: Parser0[List[ContentType.Parameter]] =
      (whitespace.with1 *> semicolon *> whitespace *> parameter).rep0

    val root: Parser0[ContentType] = ((token <* slash) ~ token ~ parameters).map { case ((tpe, subtype), parameters) =>
      ContentType(tpe, subtype, parameters)
    }
