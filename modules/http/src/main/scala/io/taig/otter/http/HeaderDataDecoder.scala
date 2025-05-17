// package io.taig.otter.http

// import cats.data.Validated
// import cats.syntax.all.*
// import io.taig.otter.Violation
// import io.taig.otter.Violations
// import io.taig.otter.collectFirstWithRemainders

// object HeaderDataDecoder:
//   object Remainders:
//     def apply[A](header: Header[A], data: Headers.Data): Validated[Violations, (Headers.Data, A)] =
//       header match
//         case Header.Root(name, codec, metadata) =>
//           val (remainders, value) = data.collectFirstWithRemainders { case (`name`, value) => value }

//           value
//             .toValid(Violations.rootNec(Violation.required))
//             .andThen: value =>
//               val explode = metadata.get(HttpKeys.explode).getOrElse(false)
//               HttpHeaderParser(explode)(codec = codec.value, value).tupleLeft(remainders)
//             .leftMap(s"$name" /: _)
//         case Header.Optional(self) =>
//           if data.exists((name, _) => name === header.name)
//           then apply(header = self, data).map(_.map(_.some))
//           else (data, none).valid
//         case Header.Modify(self, f, _) => apply(header = self, data).map(_.map(f))
