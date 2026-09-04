package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Absence
import io.taig.otter.Json

/** Writes a field, reading off its annotation what an absent value renders as.
  *
  * The annotation is why this cannot be a `contramapK` onto [[FieldEncoder]]: unwrapping the node to hand it over is
  * exactly what would throw the metadata away.
  */
object JsonFieldBorerEncoder extends Encoder[Json.Field.Node, BorerWrite]:
  private val omitting: FieldEncoder[Json.Node, BorerWrite, BorerWrite] =
    FieldEncoder(JsonBorerEncoder, absent = none, JsonFieldBorerEncoder.member)

  private val nulling: FieldEncoder[Json.Node, BorerWrite, BorerWrite] =
    FieldEncoder(JsonBorerEncoder, absent = BorerWrite(_.writeNull()).some, JsonFieldBorerEncoder.member)

  override def encode[W](json: Json.Field.Node[W, Any], w: W): BorerWrite =
    val encoder = Json.absence(json.self.metadata) match
      case Absence.Empty => nulling
      case Absence.Omit  => omitting

    encoder.encode(json.self.self, w)

  /** A member of an object: the key, then whatever the value writes. This is the pair a document model would have
    * allocated, and there is no pair.
    */
  private def member(name: String, value: BorerWrite): BorerWrite =
    BorerWrite(writer => value.write(writer.writeString(name)))
