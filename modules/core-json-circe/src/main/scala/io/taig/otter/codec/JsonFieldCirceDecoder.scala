package io.taig.otter.codec

import cats.data.Validated
import io.circe.Json as CirceJson
import io.taig.otter.Absence
import io.taig.otter.Json
import io.taig.otter.Tolerance
import io.taig.otter.Violations

/** Reads a field, reading off its annotation which forms of absence it accepts.
  *
  * A lenient field takes a missing key and an explicit `null` alike, so that it round trips whichever way it is
  * written. A strict one takes only the form it writes, which is what makes a field holding `Option[Option[A]]` able to
  * tell its two layers apart again.
  */
object JsonFieldCirceDecoder extends Decoder.Remaining[Json.Field.Node, Fields[CirceJson]]:
  private val lenient: FieldDecoder[Json.Node, CirceJson] =
    FieldDecoder(JsonCirceDecoder, absent = _.forall(_.isNull))

  private val omitted: FieldDecoder[Json.Node, CirceJson] = FieldDecoder(JsonCirceDecoder, absent = _.isEmpty)

  private val nulled: FieldDecoder[Json.Node, CirceJson] =
    FieldDecoder(JsonCirceDecoder, absent = _.exists(_.isNull))

  override def decodeRemaining[R](
      json: Json.Field.Node[Nothing, R],
      values: Fields[CirceJson]
  ): Validated[Violations, (Fields[CirceJson], R)] =
    val metadata = json.self.metadata

    val decoder = Json.tolerance(metadata) match
      case Tolerance.Lenient => lenient
      case Tolerance.Strict  =>
        Json.absence(metadata) match
          case Absence.Empty => nulled
          case Absence.Omit  => omitted

    decoder.decodeRemaining(json.self.self, values)
