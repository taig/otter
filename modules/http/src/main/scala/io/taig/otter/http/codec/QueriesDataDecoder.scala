package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Queries

import scala.collection.immutable.SortedSet

object QueriesDataDecoder extends Decoder.Remaining[Queries, Queries.Data]:
  override def decodeRemaining[A](schema: Queries[A], value: Queries.Data): Validated[Violations, (Queries.Data, A)] =
    decodeRemaining(schema = schema.value, value)

  def decodeRemaining[A](schema: Queries.Value[A], value: Queries.Data): Validated[Violations, (Queries.Data, A)] =
    schema match
      case Queries.Value.Default(self, default) =>
        val keys = SortedSet.from(value.map((key, _) => key).toIterable)
        if self.toChain.map(_.name).exists(keys.contains)
        then decodeRemaining(schema = self, value)
        else (value, default).valid
      case Queries.Value.Empty              => (value, ()).valid
      case Queries.Value.Modify(self, f, _) => decodeRemaining(schema = self, value).map(_.map(f))
      case Queries.Value.Optional(self)     =>
        val keys = SortedSet.from(value.map((key, _) => key).toIterable)
        if self.toChain.map(_.name).exists(keys.contains)
        then decodeRemaining(schema = self, value).map(_.map(_.some))
        else (value, none).valid
      case Queries.Value.Root(query)      => QueryDataDecoder.decodeRemaining(query, value)
      case Queries.Value.Zip(left, right) =>
        decodeRemaining(schema = left, value) match
          case Validated.Valid((values, a))  => decodeRemaining(schema = right, value).map(_.tupleLeft(a))
          case Validated.Invalid(violations) =>
            decodeRemaining(schema = right, value).fold(violations.combine, _ => violations).invalid
