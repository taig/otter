package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Csv

val CsvFieldEncoder: Encoder[Csv.Field.Node, Chain[(String, String)]] =
  FieldEncoder(CsvCellEncoder, empty = "")
    .contramapK([w, r] => (field: Csv.Field.Node[w, r]) => field.self.self)
