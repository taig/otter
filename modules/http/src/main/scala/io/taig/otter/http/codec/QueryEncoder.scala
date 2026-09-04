package io.taig.otter.http.codec

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Absence
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.FieldEncoder
import io.taig.otter.http.Http
import io.taig.otter.http.Query

/** Writes a query parameter as the values it contributes under its name.
  *
  * The metadata decides what an absent parameter contributes, so this cannot reach the underlying field through
  * `contramapK` -- that would throw the annotation away. Two field encoders are built once and chosen between instead,
  * exactly as the JSON interpreters do.
  */
object QueryEncoder extends Encoder[Query.Node, Chain[(String, Chain[String])]]:
  private val omitting = FieldEncoder(ParameterEncoder.Repeated, absent = none)

  private val emptying = FieldEncoder(ParameterEncoder.Repeated, absent = Chain.one("").some)

  override def encode[W](query: Query.Node[W, Any], w: W): Chain[(String, Chain[String])] =
    val encoder = Http.absence(query.self.metadata) match
      case Absence.Omit  => omitting
      case Absence.Empty => emptying

    encoder.encode(query.self.self, w)
