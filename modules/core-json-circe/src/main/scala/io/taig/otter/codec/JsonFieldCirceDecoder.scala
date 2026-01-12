package io.taig.otter.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json as CirceJson
import io.taig.otter.Field
import io.taig.otter.Json
import io.taig.otter.Violations

object JsonFieldCirceDecoder extends Decoder.Remaining[Json.Field, Chain[(String, CirceJson)]]:
  override def decodeRemaining[B](field: Json.Field[B], a: Chain[(String, CirceJson)]): Validated[Violations, (Chain[(String, CirceJson)], B)] =
    decodeRemaining(field = field.self.self, a)

  def decodeRemaining[B](field: Field[Json, B], values: Chain[(String, CirceJson)]): Validated[Violations, (Chain[(String, CirceJson)], B)] =
    field match
      case Field.Modify(self, f, g) =>  decodeRemaining(field = self, values).map(_.map(f))
      case Field.Root(name, schema) => ??? // JsonCirceDecoder.decode(schema, )
    

// val JsonFieldCirceDecoder: Decoder.Remaining[Json.Field, Chain[(String, CirceJson)]] =
//   FieldDecoder(decoder = JsonCirceDecoder).contramapK([A] => (json: Json.Field[A]) => json.self.self)
