package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Csv
import io.taig.otter.Violations

object CsvPrimitiveDecoder extends Decoder[Csv.Primitive.Node, String]:
  override def decode[R](csv: Csv.Primitive.Node[Nothing, R], value: String): Validated[Violations, R] = csv match
    case Csv.Primitive.Boolean.Schema(annotation) => PrimitiveTextDecoder.decode(annotation.self, value)
    case Csv.Primitive.Number.Schema(annotation)  => PrimitiveTextDecoder.decode(annotation.self, value)
    case Csv.Primitive.Text.Schema(annotation)    => PrimitiveTextDecoder.decode(annotation.self, value)
