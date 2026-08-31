package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Csv

/** Names the columns of a keyed row.
  *
  * A header is the one thing a CSV file says that no value takes part in, which is exactly what a [[Renderer]] is for.
  * It needs no fold of its own: `Record.fields` concatenates left before right, the same order the encoder writes its
  * cells in, and `Field.name` reaches through `optional` and `optional(default)` alike.
  */
val CsvHeaderRenderer: Renderer[Csv.Record.Node, Chain[String]] =
  Renderer([w, r] => (csv: Csv.Record.Node[w, r]) => csv.self.self.fields.map(_.value.self.self.name))
