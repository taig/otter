package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.*
import io.taig.otter.http.Header

/** Reads missing and empty values according to the field's structural contract. */
val HeaderDecoder: Decoder.Remaining[Header.Node, Fields[Chain[String]]] =
  FieldDecoder(ParameterDecoder.Delimited, isEmpty = _.forall(_.isEmpty))
    .contramapK([w, r] => (field: Header.Node[w, r]) => field.self.self)
