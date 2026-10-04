package io.taig.otter.codec

import io.taig.otter.Csv

/** Reads missing and empty values according to the field's structural contract. */
val CsvFieldDecoder: Decoder.Remaining[Csv.Field.Node, Fields[String]] =
  FieldDecoder(CsvCellDecoder, isEmpty = _.isEmpty)
    .contramapK([w, r] => (field: Csv.Field.Node[w, r]) => field.self.self)
