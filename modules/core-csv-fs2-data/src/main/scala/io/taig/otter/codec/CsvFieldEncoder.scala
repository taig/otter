package io.taig.otter.codec

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Absence
import io.taig.otter.Csv

/** Writes a column, reading off its annotation what an absent value renders as.
  *
  * A row owes its header a cell for every column, so the ordinary answer is an empty one and the column stays. Dropping
  * it shortens the row, which only a reader that is not lining up against a header can make sense of.
  *
  * The annotation is why this cannot be a `contramapK` onto [[FieldEncoder]]: unwrapping the node to hand it over is
  * exactly what would throw the metadata away.
  */
object CsvFieldEncoder extends Encoder[Csv.Field.Node, Chain[(String, String)]]:
  private val blanking: FieldEncoder[Csv.Cell.Node, String, Chain[(String, String)]] =
    FieldEncoder(CsvCellEncoder, absent = "".some)

  private val omitting: FieldEncoder[Csv.Cell.Node, String, Chain[(String, String)]] =
    FieldEncoder(CsvCellEncoder, absent = none)

  override def encode[W](csv: Csv.Field.Node[W, Any], w: W): Chain[(String, String)] =
    val encoder = Csv.absence(csv.self.metadata) match
      case Absence.Empty => blanking
      case Absence.Omit  => omitting

    encoder.encode(csv.self.self, w)
