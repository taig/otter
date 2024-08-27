package io.taig.otter.http.csv

import cats.Show

final case class Csv(headers: Option[List[String]], values: List[List[String]]):
  override def toString: String = Printers.csv(this)

object Csv:
  given Show[Csv] = Show.fromToString
