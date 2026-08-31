package io.taig.otter.codec

import cats.data.Validated
import io.taig.data.syntax.*
import io.taig.otter.Csv
import io.taig.otter.Violations

/** Reads whatever fits in one cell.
  *
  * There are no structural guards to mirror the `array` and `obj` ones a JSON decoder needs: a cell is text, so it can
  * hold the wrong value but never the wrong shape.
  */
object CsvCellDecoder extends Decoder[Csv.Cell.Node, String]:
  override def decode[R](csv: Csv.Cell.Node[Nothing, R], value: String): Validated[Violations, R] = csv match
    case Csv.Coerce.Schema(node)   => CsvCoerceDecoder.decode(node.self, value)
    case Csv.Constant.Schema(node) =>
      ConstantDecoder(CsvPrimitiveDecoder, CsvPrimitiveEncoder, _.asData).decode(node.self, value)
    case Csv.Enumeration.Schema(node) =>
      EnumerationDecoder(CsvPrimitiveDecoder, CsvPrimitiveEncoder, _.asData).decode(node.self, value)
    case Csv.Optional.Schema(node)             => OptionalDecoder(this, empty = _.isEmpty).decode(node.self, value)
    case csv @ Csv.Primitive.Boolean.Schema(_) => CsvPrimitiveDecoder.decode(csv, value)
    case csv @ Csv.Primitive.Number.Schema(_)  => CsvPrimitiveDecoder.decode(csv, value)
    case csv @ Csv.Primitive.Text.Schema(_)    => CsvPrimitiveDecoder.decode(csv, value)
