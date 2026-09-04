package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import io.taig.otter.Absence
import io.taig.otter.Tolerance
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.FieldDecoder
import io.taig.otter.codec.Fields
import io.taig.otter.http.Http
import io.taig.otter.http.Query

/** Reads a query parameter out of the values a query string has left to read.
  *
  * What counts as absent is read off [[QueryEncoder]], which is the definition of the wire. A lenient parameter takes a
  * missing name and a name given without a value alike, so `?page=` round trips as nothing whichever way it was
  * written; a strict one takes only the form its [[Absence]] asks for.
  */
object QueryDecoder extends Decoder.Remaining[Query.Node, Fields[Chain[String]]]:
  private val lenient = FieldDecoder(ParameterDecoder.Repeated, absent = _.forall(_.forall(_.isEmpty)))

  private val omitted = FieldDecoder(ParameterDecoder.Repeated, absent = _.isEmpty)

  private val emptied = FieldDecoder(ParameterDecoder.Repeated, absent = _.exists(_.forall(_.isEmpty)))

  override def decodeRemaining[R](
      query: Query.Node[Nothing, R],
      values: Fields[Chain[String]]
  ): Validated[Violations, (Fields[Chain[String]], R)] =
    val decoder = Http.tolerance(query.self.metadata) match
      case Tolerance.Lenient => lenient
      case Tolerance.Strict  =>
        Http.absence(query.self.metadata) match
          case Absence.Omit  => omitted
          case Absence.Empty => emptied

    decoder.decodeRemaining(query.self.self, values)
