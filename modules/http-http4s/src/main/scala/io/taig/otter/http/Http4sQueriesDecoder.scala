package io.taig.otter.http

import cats.data.Validated
import io.taig.otter.Violations
import org.http4s.Query as Http4sQuery
import cats.syntax.all.*

object Http4sQueriesDecoder:
  def apply[A](
      queries: Queries[A],
      values: List[Http4sQuery.KeyValue]
  ): Validated[Violations, (List[Http4sQuery.KeyValue], A)] = queries match
    case Queries.Empty              => (values, ()).valid
    case Queries.Root(query)        => Http4sQueryDecoder(query, values)
    case Queries.Modify(self, f, _) => apply(queries = self, values).map(_.map(f))
    case Queries.Optional(self) =>
      val names = self.toChain.map(_.name)
      val keys = values.map((key, _) => key).toSet
      if names.exists(keys.contains_)
      then apply(queries = self, values).map(_.map(_.some))
      else (values, none).valid
    case Queries.Zip(left, right) =>
      apply(queries = left, values) match
        case Validated.Valid((values, a)) => apply(queries = right, values).map(_.tupleLeft(a))
        case Validated.Invalid(violations) =>
          apply(queries = right, values).fold(violations.combine, _ => violations).invalid
