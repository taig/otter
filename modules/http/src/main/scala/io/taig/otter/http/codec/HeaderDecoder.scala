package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import io.taig.otter.Absence
import io.taig.otter.Tolerance
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.FieldDecoder
import io.taig.otter.codec.Fields
import io.taig.otter.http.Header
import io.taig.otter.http.Http

/** Reads a header out of the lines a header set has left to read.
  *
  * What counts as absent is read off [[HeaderEncoder]], the same way [[QueryDecoder]] reads it off [[QueryEncoder]]: a
  * lenient header takes a missing name and a name sent with nothing after the colon alike.
  */
object HeaderDecoder extends Decoder.Remaining[Header.Node, Fields[Chain[String]]]:
  private val lenient = FieldDecoder(ParameterDecoder.Delimited, absent = _.forall(_.forall(_.isEmpty)))

  private val omitted = FieldDecoder(ParameterDecoder.Delimited, absent = _.isEmpty)

  private val emptied = FieldDecoder(ParameterDecoder.Delimited, absent = _.exists(_.forall(_.isEmpty)))

  override def decodeRemaining[R](
      header: Header.Node[Nothing, R],
      values: Fields[Chain[String]]
  ): Validated[Violations, (Fields[Chain[String]], R)] =
    val decoder = Http.tolerance(header.self.metadata) match
      case Tolerance.Lenient => lenient
      case Tolerance.Strict  =>
        Http.absence(header.self.metadata) match
          case Absence.Omit  => omitted
          case Absence.Empty => emptied

    decoder.decodeRemaining(header.self.self, values)
