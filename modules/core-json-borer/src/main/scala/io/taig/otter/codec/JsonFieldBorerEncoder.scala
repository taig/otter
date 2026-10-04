package io.taig.otter.codec

import io.taig.otter.Json

val JsonFieldBorerEncoder: Encoder[Json.Field.Node, BorerWrite] =
  FieldEncoder(
    JsonBorerEncoder,
    empty = BorerWrite(_.writeNull()),
    (name: String, value: BorerWrite) => BorerWrite(writer => value.write(writer.writeString(name)))
  )
    .contramapK([w, r] => (field: Json.Field.Node[w, r]) => field.self.self)
