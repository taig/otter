package io.taig.otter.codec

import io.taig.otter.Json

/** Writes a JSON text schema as the text itself, which is what a dictionary key and a field's name are made of. Free of
  * any particular JSON library, because there is no JSON here: the result is the key, not a document.
  */
object JsonTextEncoder extends Encoder[Json.Primitive.Text.Node, String]:
  override def encode[W](json: Json.Primitive.Text.Node[W, Any], w: W): String =
    PrimitiveTextEncoder.encode(json.self.self, w)
