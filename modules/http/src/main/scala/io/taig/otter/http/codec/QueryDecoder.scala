package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.*
import io.taig.otter.http.Query

/** Reads missing and empty values according to the field's structural contract. */
val QueryDecoder: Decoder.Remaining[Query.Node, Fields[Chain[String]]] =
  FieldDecoder(ParameterDecoder.Repeated, isEmpty = _.forall(_.isEmpty))
    .contramapK([w, r] => (field: Query.Node[w, r]) => field.self.self)
