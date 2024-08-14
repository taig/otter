package io.taig.otter.http.csv

import cats.syntax.all.*
import fs2.Stream

private object Printers:
  val cell: Cell => String =
    case Cell(value, true)  => s"\"$value\""
    case Cell(value, false) => value

  val row: Row => String = _.toList.map(cell).mkString(",")

  val rows: List[Row] => String = _.map(row).mkString("\n")

  val labels: List[String] => String = _.map(value => s"\"$value\"").mkString(",")

  val strict: Csv.Strict => String = csv => csv.labels.map(labels).map(_ + "\n").orEmpty + rows(csv.rows)

  def streaming[F[_]]: Csv.Streaming[F] => Stream[F, String] = csv =>
    (Stream.fromOption(csv.labels.map(labels)) ++ csv.rows.map(row)).intersperse("\n")
