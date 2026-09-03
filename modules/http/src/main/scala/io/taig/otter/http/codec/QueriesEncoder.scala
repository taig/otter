package io.taig.otter.http.codec

import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.RecordEncoder
import io.taig.otter.http.Queries

/** Writes a query string as its name and value pairs, in the order the schema names them.
  *
  * The pairs are `Option`al on the value side because that is what a query string can say -- `?a=1`, `?a=`, `?a` -- and
  * reading gives all three a meaning. Writing only ever produces the first two: a name worth giving is a name worth
  * giving a value to, even an empty one, and a parameter with nothing to say is left out by [[QueryEncoder]] instead.
  * Percent encoding and the `?` and `&` between the pairs are the backend's, which owns the URL type it is building.
  */
val QueriesEncoder: Encoder[Queries.Node, Chain[(String, Option[String])]] =
  RecordEncoder(QueryEncoder)
    .contramapK[Queries.Node]([w, r] => (queries: Queries.Node[w, r]) => queries.self.self)
    .map(_.flatMap((name, values) => values.map(value => (name, value.some))))
