package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.*
import io.taig.otter.http.Query

val QueryEncoder: Encoder[Query.Node, Chain[(String, Chain[String])]] =
  FieldEncoder(ParameterEncoder.Repeated, empty = Chain.one(""))
    .contramapK([w, r] => (field: Query.Node[w, r]) => field.self.self)
