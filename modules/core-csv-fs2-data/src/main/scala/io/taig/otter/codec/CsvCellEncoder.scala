package io.taig.otter.codec

import io.taig.otter.Csv

/** Writes whatever fits in one cell. Nothing here nests, so the only recursion is an optional cell's own. */
object CsvCellEncoder extends Encoder[Csv.Cell.Node, String]:
  override def encode[W](csv: Csv.Cell.Node[W, Any], w: W): String = csv match
    case Csv.Coerce.Schema(node)               => CoerceEncoder(CsvPrimitiveEncoder).encode(node.self, w)
    case Csv.Constant.Schema(node)             => ConstantEncoder(CsvPrimitiveEncoder).encode(node.self, w)
    case Csv.Enumeration.Schema(node)          => EnumerationEncoder(CsvPrimitiveEncoder).encode(node.self, w)
    case Csv.Optional.Schema(node)             => OptionalEncoder(this, empty = "").encode(node.self, w)
    case csv @ Csv.Primitive.Boolean.Schema(_) => CsvPrimitiveEncoder.encode(csv, w)
    case csv @ Csv.Primitive.Number.Schema(_)  => CsvPrimitiveEncoder.encode(csv, w)
    case csv @ Csv.Primitive.Text.Schema(_)    => CsvPrimitiveEncoder.encode(csv, w)
