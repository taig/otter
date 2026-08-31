package io.taig.otter.codec

import io.taig.otter.Csv

/** Writes a positional row. An absent cell is an empty one, exactly as in a keyed row, because a headerless row still
  * has to keep its width for the cells after it to stay where they are.
  */
val CsvTupleEncoder: Encoder[Csv.Tuple.Node, Vector[String]] =
  TupleEncoder(CsvCellEncoder, empty = "").contramapK([w, r] => (csv: Csv.Tuple.Node[w, r]) => csv.self.self)
