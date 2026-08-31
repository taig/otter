package io.taig.otter.codec

import cats.data.Chain
import cats.data.Validated
import io.taig.otter.Absence
import io.taig.otter.Csv
import io.taig.otter.Tolerance
import io.taig.otter.Violations

/** Reads a column, reading off its annotation which forms of absence it accepts.
  *
  * A lenient column takes a missing column and an empty cell alike, so that it round trips whichever way it is written.
  * A strict one takes only the form it writes. The three predicates are the same three JSON uses, with an empty cell
  * standing where `null` stands there — which is the whole of what changes between the two formats here.
  */
object CsvFieldDecoder extends Decoder.Remaining[Csv.Field.Node, Chain[(String, String)]]:
  private val lenient: FieldDecoder[Csv.Cell.Node, String] =
    FieldDecoder(CsvCellDecoder, absent = _.forall(_.isEmpty))

  private val blanked: FieldDecoder[Csv.Cell.Node, String] =
    FieldDecoder(CsvCellDecoder, absent = _.exists(_.isEmpty))

  private val omitted: FieldDecoder[Csv.Cell.Node, String] = FieldDecoder(CsvCellDecoder, absent = _.isEmpty)

  override def decodeRemaining[R](
      csv: Csv.Field.Node[Nothing, R],
      values: Chain[(String, String)]
  ): Validated[Violations, (Chain[(String, String)], R)] =
    val metadata = csv.self.metadata

    val decoder = Csv.tolerance(metadata) match
      case Tolerance.Lenient => lenient
      case Tolerance.Strict  =>
        Csv.absence(metadata) match
          case Absence.Empty => blanked
          case Absence.Omit  => omitted

    decoder.decodeRemaining(csv.self.self, values)
