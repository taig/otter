// package io.taig.otter.http

// import cats.data.Chain

// object QueriesDataEncoder:
//   def apply[A](queries: Queries[A], a: A): Queries.Data = queries match
//     case Queries.Empty              => Chain.empty
//     case Queries.Root(query)        => QueryDataEncoder(query, a)
//     case Queries.Modify(self, _, g) => apply(queries = self, g(a))
//     case Queries.Optional(self)     => a.map(apply(queries = self, _)).getOrElse(Chain.empty)
//     case Queries.Zip(left, right)   => apply(queries = left, a._1) ++ apply(queries = right, a._2)
