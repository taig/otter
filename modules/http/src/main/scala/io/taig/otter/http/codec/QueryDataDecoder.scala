package io.taig.otter.http.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Queries
import io.taig.otter.http.Query
import io.taig.otter.partitionMap

object QueryDataDecoder extends Decoder.Remaining[Query, Queries.Data]:
  override def decodeRemaining[A](schema: Query[A], values: Queries.Data): Validated[Violations, (Queries.Data, A)] =
    decodeRemaining(schema = schema.value, values)

  def decodeRemaining[A](schema: Query.Value[A], values: Queries.Data): Validated[Violations, (Queries.Data, A)] =
    schema match
      case Query.Value.Modify(self, f, _) => decodeRemaining(schema = self, values).map(_.map(f))
      case Query.Value.Optional(self)     =>
        if values.exists((key, _) => key === self.name)
        then decodeRemaining(schema = self, values).map(_.map(_.some))
        else (values, none).valid
      case Query.Value.Root(name, schema, explode, style) =>
        val (remainders, results) = values.partitionMap: (key, value) =>
          Either.cond(key === name, right = value, left = (key, value))

        QuerySchemaDecoder(explode, style)
          .decodeRemaining(schema = schema.value, values = results)
          .leftMap(name /: _)
          .map((remainders, a) => (remainders.tupleLeft(name), a))
