package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.*
import io.taig.otter.http.Header

val HeaderEncoder: Encoder[Header.Node, Chain[(String, Chain[String])]] =
  FieldEncoder(ParameterEncoder.Delimited, empty = Chain.one(""))
    .contramapK([w, r] => (field: Header.Node[w, r]) => field.self.self)
