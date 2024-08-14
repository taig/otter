package io.taig.otter.http.csv

import cats.parse.Parser

final case class Csv(labels: Option[List[String]], rows: List[Row])

object Csv:
  def parse(value: String): Either[Parser.Error, Csv] = Parsers.csv.parseAll(value)
