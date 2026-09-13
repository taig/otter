package io.taig.otter.http

import io.taig.otter.Csv
import io.taig.otter.Reference

/** A CSV document is either exactly one data row or a finite collection of rows. */
sealed abstract class CsvDocument[-W, +R]

object CsvDocument:
  sealed abstract class Row[-W, +R] extends CsvDocument[W, R]

  /** Named columns, with one header row per document. */
  final case class Record[-W, +R](schema: Reference[Csv.Record.Node, W, R]) extends CsvDocument.Row[W, R]

  /** Positional columns, without a header. */
  final case class Tuple[-W, +R](schema: Reference[Csv.Tuple.Node, W, R]) extends CsvDocument.Row[W, R]

  final case class Rows[-W, +R](schema: Reference[[w, r] =>> CsvDocument.Row[w, r], W, R])
      extends CsvDocument[Vector[W], Vector[R]]
