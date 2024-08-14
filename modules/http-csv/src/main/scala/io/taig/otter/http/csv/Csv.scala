package io.taig.otter.http.csv

import cats.parse.Parser
import fs2.Stream

final case class Csv[F[_]](labels: Option[List[String]], rows: F[Row])

object Csv:
  type Strict = Csv[List]
  type Streaming[F[_]] = Csv[Stream[F, *]]

  def parse(value: String): Either[Parser.Error, Csv.Strict] = Parsers.csv.parseAll(value)
