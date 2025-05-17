// package io.taig.otter.http

// import cats.data.Validated
// import cats.syntax.all.*
// import io.taig.otter.Violation
// import io.taig.otter.Violations

// object UrlDataDecoder:
//   def apply[A](url: Url[A], data: Url.Data): Validated[Violations, A] =
//     Remainders(url, data).andThen: (data, a) =>
//       Validated.cond(
//         test = data.path.isEmpty,
//         a,
//         Violations.rootNec(Violation.equal(reference = "/", actual = "/" + data.path.mkString_("/")))
//       )

//   object Remainders:
//     def apply[A](url: Url[A], data: Url.Data): Validated[Violations, (Url.Data, A)] = url match
//       case Url.Empty              => (data, ()).valid
//       case Url.Modify(self, f, _) => apply(url = self, data).map(_.map(f))
//       case Url.Root(path, queries) =>
//         PathDataDecoder
//           .Remainders(path, data = data.path)
//           .andThen: (path, a) =>
//             QueriesDataDecoder
//               .Remainders(queries, data = data.queries)
//               .map((queries, b) => (Url.Data(path, queries), (a, b)))
//       case Url.Zip(left, right) =>
//         apply(url = left, data).andThen: (data, a) =>
//           apply(url = right, data).map((data, b) => (data, (a, b)))
