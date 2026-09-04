package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.RecordEncoder
import io.taig.otter.http.Headers

/** Writes a header set as its name and value lines, spelled the way the schema names them.
  *
  * A header name is case insensitive on the wire, so writing has a free choice and takes the schema's: what a
  * definition says is what goes out, and [[HeadersDecoder]] is where the other spellings are accounted for.
  */
val HeadersEncoder: Encoder[Headers.Node, Chain[(String, String)]] =
  RecordEncoder(HeaderEncoder)
    .contramapK[Headers.Node]([w, r] => (headers: Headers.Node[w, r]) => headers.self.self)
    .map(_.flatMap((name, values) => values.map(name -> _)))
