package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Queries

import scala.collection.immutable.SortedSet

object QueriesDataDecoder extends Decoder.Remainding[Queries, Queries.Data]:
  override def decodeRemainding[A](schema: Queries[A], value: Queries.Data): Validated[Violations, (Queries.Data, A)] =
    decodeRemainding(schema = schema.self, value)

  def decodeRemainding[A](schema: Queries.Value[A], value: Queries.Data): Validated[Violations, (Queries.Data, A)] =
    schema match
      case Queries.Value.Empty              => (value, ()).valid
      case Queries.Value.Root(query)        => QueryDataDecoder.decodeRemainding(query, value)
      case Queries.Value.Modify(self, f, _) => decodeRemainding(schema = self, value).map(_.map(f))
      case Queries.Value.Optional(self) =>
        val keys = SortedSet.from(value.map((key, _) => key).toIterable)
        if self.toChain.map(_.name).exists(keys.contains)
        then decodeRemainding(schema = self, value).map(_.map(_.some))
        else (value, none).valid
      case Queries.Value.Zip(left, right) =>
        decodeRemainding(schema = left, value) match
          case Validated.Valid((values, a)) => decodeRemainding(schema = right, value).map(_.tupleLeft(a))
          case Validated.Invalid(violations) =>
            decodeRemainding(schema = right, value).fold(violations.combine, _ => violations).invalid
