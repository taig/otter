package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Json
import io.taig.otter.Violations

/** Reads a JSON text schema out of the text itself, which is the inverse of what [[JsonTextEncoder]] writes. */
object JsonTextDecoder extends Decoder[Json.Primitive.Text.Node, String]:
  override def decode[R](json: Json.Primitive.Text.Node[Nothing, R], value: String): Validated[Violations, R] =
    PrimitiveTextDecoder.decode(json.self.self, value)
