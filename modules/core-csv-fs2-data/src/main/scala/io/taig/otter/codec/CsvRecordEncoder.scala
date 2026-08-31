package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Csv

/** Writes a keyed row as the columns it is made of. */
val CsvRecordEncoder: Encoder[Csv.Record.Node, Chain[(String, String)]] =
  RecordEncoder(CsvFieldEncoder).contramapK([w, r] => (csv: Csv.Record.Node[w, r]) => csv.self.self)
