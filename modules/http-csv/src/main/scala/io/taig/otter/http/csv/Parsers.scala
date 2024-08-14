package io.taig.otter.http.csv

import cats.parse.Parser
import cats.parse.strings.Json
import cats.parse.Rfc5234
import cats.parse.Parser0

private object Parsers:
  val whitespace: Parser0[Unit] = Parser.charIn(" \t\r\n").rep0.void

  val separator: Parser0[Unit] = whitespace *> Parser.char(',') <* whitespace

  val cell: Parser[Cell] = Json.delimited.parser.map(Cell(_, quoted = true)) |
    Parser.charsWhile(value => value != ',' && value != '\n').map(Cell(_, quoted = false))

  val row: Parser[Row] = Parser.repSep(cell, separator).map(values => Row(values.toList))

  val rows0: Parser0[List[Row]] = row.repSep0(Rfc5234.crlf)

  val labels: Parser[List[String]] = Json.delimited.parser.repSep(separator).map(_.toList)

  val csv: Parser0[Csv] = (labels.? ~ rows0).map(Csv.apply)
