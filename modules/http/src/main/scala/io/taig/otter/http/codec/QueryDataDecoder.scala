package io.taig.otter.http.codec

import io.taig.otter.codec.Decoder
import io.taig.otter.http.Queries
import cats.data.Validated
import io.taig.otter.partitionMap
import io.taig.otter.Violations
import io.taig.otter.http.Query
import cats.syntax.all.*
import cats.data.Chain

object QueryDataDecoder extends Decoder.Remainding[Query, Queries.Data]:
  override def decodeRemainding[A](schema: Query[A], values: Queries.Data): Validated[Violations, (Queries.Data, A)] =
    schema match
      case Query.Modify(self, f, _) => decodeRemainding(schema = self, values).map(_.map(f))
      case Query.Optional(self) =>
        if values.exists((key, _) => key === self.name)
        then decodeRemainding(schema = self, values).map(_.map(_.some))
        else (values, none).valid
      case Query.Root(name, schema, explode, style, _) =>
        val (remainders, results) = values.partitionMap: (key, value) =>
          Either.cond(key === name, right = value, left = (key, value))

        HttpQueryDecoder(explode, style)
          .decodeRemainding(schema = schema.value, values = results)
          .leftMap(name /: _)
          .map((remainders, a) => (remainders.tupleLeft(name), a))
