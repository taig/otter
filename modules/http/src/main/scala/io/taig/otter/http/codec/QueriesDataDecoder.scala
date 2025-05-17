package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.codec.QueryDataDecoder
import scala.collection.immutable.SortedSet

object QueriesDataDecoder extends Decoder.Remainding[Queries, Queries.Data]:
  override def decodeRemainding[A](schema: Queries[A], value: Queries.Data): Validated[Violations, (Queries.Data, A)] =
    schema match
      case Queries.Empty              => (value, ()).valid
      case Queries.Root(query)        => QueryDataDecoder.decodeRemainding(query, value)
      case Queries.Modify(self, f, _) => decodeRemainding(schema = self, value).map(_.map(f))
      case Queries.Optional(self) =>
        val keys = SortedSet.from(value.map((key, _) => key).toIterable)
        if self.toChain.map(_.name).exists(keys.contains)
        then decodeRemainding(schema = self, value).map(_.map(_.some))
        else (value, none).valid
      case Queries.Zip(left, right) =>
        decodeRemainding(schema = left, value) match
          case Validated.Valid((values, a)) => decodeRemainding(schema = right, value).map(_.tupleLeft(a))
          case Validated.Invalid(violations) =>
            decodeRemainding(schema = right, value).fold(violations.combine, _ => violations).invalid
