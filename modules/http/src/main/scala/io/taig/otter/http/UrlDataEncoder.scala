// package io.taig.otter.http

// object UrlDataEncoder:
//   def apply[A](url: Url[A], a: A): Url.Data = url match
//     case Url.Empty              => Url.Data.Empty
//     case Url.Modify(self, _, g) => apply(url = self, g(a))
//     case Url.Root(path, queries) =>
//       Url.Data(
//         path = PathDataEncoder(path, a._1),
//         queries = QueriesDataEncoder(queries, a._2)
//       )
//     case Url.Zip(left, right) =>
//       apply(url = left, a._1).combine(apply(url = right, a._2))
