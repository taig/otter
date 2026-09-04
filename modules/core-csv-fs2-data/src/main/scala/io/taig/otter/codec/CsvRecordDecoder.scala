package io.taig.otter.codec

import io.taig.otter.Csv

/** Reads a keyed row from the columns it is made of.
  *
  * Kept as a [[Decoder.Remaining]] so that the columns no field claimed are still visible, which is what a reader that
  * rejects an unknown column needs.
  */
val CsvRecordDecoder: Decoder.Remaining[Csv.Record.Node, Fields[String]] =
  RecordDecoder(CsvFieldDecoder).contramapK([w, r] => (csv: Csv.Record.Node[w, r]) => csv.self.self)
