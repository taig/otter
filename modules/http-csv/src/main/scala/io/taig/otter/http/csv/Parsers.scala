package io.taig.otter.http.csv

import cats.parse.Parser
import cats.parse.strings.Json
import cats.parse.Rfc5234
import cats.parse.Parser0

private object Parsers:
  val token: Parser[String] = Parser.until(Rfc5234.wsp | Rfc5234.crlf | Parser.char(','))

  val header: Parser[String] = Json.delimited.parser | token

  val headers: Parser0[List[String]] = header.repSep0(Parser.char(','))
