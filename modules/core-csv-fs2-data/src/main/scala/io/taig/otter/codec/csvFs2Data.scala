package io.taig.otter.codec

import cats.data.Chain
import cats.data.NonEmptyList
import fs2.data.csv.CsvRow
import fs2.data.csv.Row
import io.taig.otter.Csv

/** Reads and writes fs2-data's own rows.
  *
  * This is where the library takes over: it owns the text, so quoting, escaping and separators are none of Otter's
  * business, exactly as circe owns them for JSON. What Otter owns is the step above, which schema a cell answers to,
  * and fs2-data's own `CellDecoder` and `CellEncoder` are therefore unused.
  *
  * The two row types line up with the two row schemas: `CsvRow` carries its headers and answers to [[Csv.Record]],
  * `Row` is positional and answers to [[Csv.Tuple]].
  *
  * Writing yields an `Option` because a row holds at least one cell where a schema need not: `RNil` and `TNil` have no
  * row to be.
  */
object CsvKeyedRowEncoder extends Encoder[Csv.Record.Node, Option[CsvRow[String]]]:
  /** Aligned to the header the schema renders rather than to the cells this one row happened to write.
    *
    * A `CsvRow` is its headers and its values together, and every row of a file has to agree with the rest on them, so
    * a column the writer dropped is filled back in as an empty cell. That is the sense in which
    * [[io.taig.otter.Absence.Omit]] says nothing here that [[io.taig.otter.Absence.Empty]] does not: a row lined up
    * against a header has no way to be missing one of its columns. Only a positional row, or a reader, can tell the two
    * apart.
    */
  override def encode[W](csv: Csv.Record.Node[W, Any], w: W): Option[CsvRow[String]] =
    val cells = CsvRecordEncoder.encode(csv, w).toList.toMap

    NonEmptyList
      .fromList(CsvHeaderRenderer.render(csv).toList)
      .flatMap(headers => CsvRow(headers.map(cells.getOrElse(_, "")), headers).toOption)

val CsvKeyedRowDecoder: Decoder[Csv.Record.Node, CsvRow[String]] =
  CsvRecordDecoder.contramap(row => Chain.fromSeq(row.headers.value.toList.zip(row.values.toList)))

val CsvRowEncoder: Encoder[Csv.Tuple.Node, Option[Row]] =
  CsvTupleEncoder.map(cells => NonEmptyList.fromList(cells.toList).map(Row(_)))

val CsvRowDecoder: Decoder[Csv.Tuple.Node, Row] = CsvTupleDecoder.contramap(_.values.toList.toVector)
