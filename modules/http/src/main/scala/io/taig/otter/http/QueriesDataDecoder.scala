package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations

object QueriesDataDecoder:
  object Remainders:
    def apply[A](queries: Queries[A], data: Queries.Data): Validated[Violations, (Queries.Data, A)] =
      queries match
        case Queries.Empty              => (data, ()).valid
        case Queries.Root(query)        => QueryDataDecoder.Remainders(query, data)
        case Queries.Modify(self, f, _) => apply(queries = self, data).map(_.map(f))
        case Queries.Optional(self) =>
          val names = self.toChain.map(_.name)
          if names.exists(name => data.exists((key, _) => name === key))
          then apply(queries = self, data).map(_.map(_.some))
          else (data, none).valid
        case Queries.Zip(left, right) =>
          apply(queries = left, data) match
            case Validated.Valid((values, a)) => apply(queries = right, data).map(_.tupleLeft(a))
            case Validated.Invalid(violations) =>
              apply(queries = right, data).fold(violations.combine, _ => violations).invalid
