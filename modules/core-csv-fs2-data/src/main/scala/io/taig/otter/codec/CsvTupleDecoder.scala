package io.taig.otter.codec

import io.taig.otter.Csv

/** Reads a positional row, rejecting one whose width is not the schema's. */
val CsvTupleDecoder: Decoder[Csv.Tuple.Node, Vector[String]] =
  TupleDecoder(CsvCellDecoder, empty = _.isEmpty).contramapK([w, r] => (csv: Csv.Tuple.Node[w, r]) => csv.self.self)
