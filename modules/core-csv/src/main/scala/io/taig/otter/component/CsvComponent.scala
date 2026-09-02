package io.taig.otter.component

import io.taig.otter.Csv
import io.taig.otter.codec.CsvTextEncoder
import io.taig.otter.syntax.AllSyntax
import io.taig.otter.syntax.CsvSyntax

/** The CSV vocabulary.
  *
  * Shorter than [[JsonComponent]] by everything CSV cannot spell: there is no `branch`, no `collection` and no
  * `dictionary`, because none of them fit in a cell. What is left is a row, keyed or positional, made of cells.
  */
trait CsvComponent
    extends AllSyntax,
      CsvSyntax,
      PrimitiveComponent.Boolean[Csv.Primitive.Boolean.Schema],
      PrimitiveComponent.Number[Csv.Primitive.Number.Schema],
      PrimitiveComponent.Text[Csv.Primitive.Text.Schema],
      RecordComponent[Csv.Cell.Node, Csv.Record.Schema, Csv.Field.Schema],
      TupleComponent[Csv.Cell.Node, Csv.Tuple.Schema]:
  object field
      extends RecordComponent.Field[Csv.Cell.Node, Csv.Primitive.Text.Node, Csv.Field.Schema](using CsvTextEncoder)
  object coerce extends CoerceComponent[Csv.Primitive.Node, Csv.Coerce.Schema]
  object constant extends ConstantComponent[Csv.Primitive.Node, Csv.Constant.Schema]
  object enumeration extends EnumerationComponent[Csv.Primitive.Node, Csv.Enumeration.Schema]

object CsvComponent extends CsvComponent
