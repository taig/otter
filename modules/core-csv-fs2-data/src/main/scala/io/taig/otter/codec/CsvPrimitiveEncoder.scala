package io.taig.otter.codec

import io.taig.otter.Csv

object CsvPrimitiveEncoder extends Encoder[Csv.Primitive.Node, String]:
  override def encode[W](csv: Csv.Primitive.Node[W, Any], w: W): String = csv match
    case Csv.Primitive.Boolean.Schema(annotation) => PrimitiveTextEncoder.encode(annotation.self, w)
    case Csv.Primitive.Number.Schema(annotation)  => PrimitiveTextEncoder.encode(annotation.self, w)
    case Csv.Primitive.Text.Schema(annotation)    => PrimitiveTextEncoder.encode(annotation.self, w)
