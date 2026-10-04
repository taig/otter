package io.taig.otter.codec

import io.bullet.borer.Dom
import io.taig.otter.Json

/** Reads missing and empty values according to the field's structural contract. */
val JsonFieldBorerDecoder: Decoder.Remaining[Json.Field.Node, Fields[Dom.Element]] =
  FieldDecoder(JsonBorerDecoder, isEmpty = _ == Dom.NullElem)
    .contramapK([w, r] => (field: Json.Field.Node[w, r]) => field.self.self)
