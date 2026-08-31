package io.taig.otter.codec

import cats.data.Chain
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.otter.Absence
import io.taig.otter.Json

/** Writes a field, reading off its annotation what an absent value renders as.
  *
  * The annotation is why this cannot be a `contramapK` onto [[FieldEncoder]]: unwrapping the node to hand it over is
  * exactly what would throw the metadata away.
  */
object JsonFieldCirceEncoder extends Encoder[Json.Field.Node, Chain[(String, CirceJson)]]:
  private val omitting: FieldEncoder[Json.Node, CirceJson] = FieldEncoder(JsonCirceEncoder, absent = none)

  private val nulling: FieldEncoder[Json.Node, CirceJson] =
    FieldEncoder(JsonCirceEncoder, absent = CirceJson.Null.some)

  override def encode[W](json: Json.Field.Node[W, Any], w: W): Chain[(String, CirceJson)] =
    val encoder = Json.absence(json.self.metadata) match
      case Absence.Empty => nulling
      case Absence.Omit  => omitting

    encoder.encode(json.self.self, w)
