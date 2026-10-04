package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json

/** Reads missing and empty values according to the field's structural contract. */
val JsonFieldCirceDecoder: Decoder.Remaining[Json.Field.Node, Fields[CirceJson]] =
  FieldDecoder(JsonCirceDecoder, isEmpty = _.isNull)
    .contramapK([w, r] => (field: Json.Field.Node[w, r]) => field.self.self)
