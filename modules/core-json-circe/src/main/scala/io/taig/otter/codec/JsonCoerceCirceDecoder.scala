package io.taig.otter.codec

import cats.data.Validated
import io.circe.Json as CirceJson
import io.taig.otter.Coerce
import io.taig.otter.Json
import io.taig.otter.Violations

/** Normalises a laxer wire representation before handing it to the primitive decoder: a quoted boolean or number is
  * accepted where one is expected, and a boolean or number is accepted where a string is expected.
  */
object JsonCoerceCirceDecoder extends Decoder[[w, r] =>> Coerce[Json.Primitive.Node, w, r], CirceJson]:
  override def decode[R](schema: Coerce[Json.Primitive.Node, Nothing, R], json: CirceJson): Validated[Violations, R] =
    schema match
      case Coerce.Modify(self, f, _) => decode(self, json).map(f)
      case Coerce.Root(reference)    =>
        val primitive = reference.value
        JsonPrimitiveCirceDecoder.decode(primitive, coerce(primitive, json))

  private def coerce[R](schema: Json.Primitive.Node[Nothing, R], json: CirceJson): CirceJson =
    schema match
      case Json.Primitive.Boolean.Schema(_) =>
        json.asString.flatMap(_.toBooleanOption).fold(json)(CirceJson.fromBoolean)
      case Json.Primitive.Number.Schema(_) =>
        json.asString.flatMap(value => CirceJson.fromString(value).asString).flatMap(parseNumber).getOrElse(json)
      case Json.Primitive.Text.Schema(_) =>
        json.asNumber
          .map(number => CirceJson.fromString(number.toString))
          .orElse(json.asBoolean.map(value => CirceJson.fromString(value.toString)))
          .getOrElse(json)

  private def parseNumber(value: String): Option[CirceJson] =
    io.circe.JsonNumber.fromString(value).map(CirceJson.fromJsonNumber)
