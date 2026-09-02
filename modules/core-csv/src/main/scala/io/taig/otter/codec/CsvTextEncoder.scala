package io.taig.otter.codec

import io.taig.otter.Csv

/** Writes a CSV text schema as the text itself, which is what a field's name -- its column header -- is made of. */
object CsvTextEncoder extends Encoder[Csv.Primitive.Text.Node, String]:
  override def encode[W](csv: Csv.Primitive.Text.Node[W, Any], w: W): String =
    PrimitiveTextEncoder.encode(csv.self.self, w)
