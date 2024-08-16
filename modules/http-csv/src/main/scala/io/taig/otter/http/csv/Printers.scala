package io.taig.otter.http.csv

private object Printers:
  val csv: Csv => String = csv =>
    (
      csv.headers.map(_.map(value => s"\"$value\"").mkString(",")).toList ++
        csv.values.map(_.map(value => s"\"$value\"").mkString(","))
    ).mkString("\n")
