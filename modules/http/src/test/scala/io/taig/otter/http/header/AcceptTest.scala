// package io.taig.otter.http.header

// import cats.data.Ior
// import cats.data.NonEmptyList
// import cats.syntax.all.*
// import munit.FunSuite
// import org.typelevel.ci.*

// final class AcceptTest extends FunSuite:
//   test("parse"):
//     assertEquals(
//       obtained = Accept.parse("text/plain"),
//       expected = Accept(
//         NonEmptyList.of(
//           Weighted(
//             self = MediaRange(
//               tpe = MediaRange.Type.Secondary("text", "plain"),
//               parameters = Parameters.Empty
//             ),
//             weight = none
//           )
//         )
//       ).asRight
//     )

//     assertEquals(
//       obtained = Accept.parse("text/plain, image/png"),
//       expected = Accept(
//         NonEmptyList.of(
//           Weighted(
//             self = MediaRange(
//               tpe = MediaRange.Type.Secondary("text", "plain"),
//               parameters = Parameters.Empty
//             ),
//             weight = none
//           ),
//           Weighted(
//             self = MediaRange(
//               tpe = MediaRange.Type.Secondary("image", "png"),
//               parameters = Parameters.Empty
//             ),
//             weight = none
//           )
//         )
//       ).asRight
//     )

//     assertEquals(
//       obtained = Accept.parse("text/plain; q=1"),
//       expected = Accept(
//         NonEmptyList.of(
//           Weighted(
//             self = MediaRange(
//               tpe = MediaRange.Type.Secondary("text", "plain"),
//               parameters = Parameters.Empty
//             ),
//             weight = BigDecimal(1).some
//           )
//         )
//       ).asRight
//     )

//   test("parse: last q"):
//     assertEquals(
//       obtained = Accept.parse("text/plain; q=0.5; q=0.7"),
//       expected = Accept(
//         NonEmptyList.of(
//           Weighted(
//             self = MediaRange(
//               tpe = MediaRange.Type.Secondary("text", "plain"),
//               parameters = Parameters.of(ci"q" -> "0.5")
//             ),
//             weight = BigDecimal("0.7").some
//           )
//         )
//       ).asRight
//     )

//   test("parse: invalid q"):
//     assertEquals(
//       obtained = Accept.parse("text/plain; q=0.5; q=foo; q=1.1; q=0.1234"),
//       expected = Accept(
//         NonEmptyList.of(
//           Weighted(
//             self = MediaRange(
//               tpe = MediaRange.Type.Secondary("text", "plain"),
//               parameters = Parameters.of(
//                 ci"q" -> "foo",
//                 ci"q" -> "1.1",
//                 ci"q" -> "0.1234"
//               )
//             ),
//             weight = BigDecimal("0.5").some
//           )
//         )
//       ).asRight
//     )

//   test("toResult"):
//     assertEquals(
//       obtained = Accept(
//         NonEmptyList.of(
//           Weighted(
//             self = MediaRange(tpe = MediaRange.Type.Primary("image"), parameters = Parameters.Empty),
//             weight = none
//           ),
//           Weighted(
//             self = MediaRange(tpe = MediaRange.Type.Primary("text"), parameters = Parameters.Empty),
//             weight = none
//           ),
//           Weighted(
//             self = MediaRange(tpe = MediaRange.Type.Secondary("text", "plain"), parameters = Parameters.Empty),
//             weight = none
//           ),
//           Weighted(
//             self = MediaRange(
//               tpe = MediaRange.Type.Secondary("text", "plain"),
//               parameters = Parameters.of(ci"foo" -> "bar")
//             ),
//             weight = none
//           ),
//           Weighted(
//             self = MediaRange(tpe = MediaRange.Type.Secondary("application", "json"), parameters = Parameters.Empty),
//             weight = BigDecimal("0.5").some
//           ),
//           Weighted(
//             self = MediaRange(tpe = MediaRange.Type.Secondary("text", "html"), parameters = Parameters.Empty),
//             weight = BigDecimal(0).some
//           ),
//           Weighted(
//             self = MediaRange(tpe = MediaRange.Type.Any, parameters = Parameters.Empty),
//             weight = BigDecimal("0.1").some
//           )
//         )
//       ).toResult,
//       expected = Ior.Both(
//         NonEmptyList.of(MediaRange(tpe = MediaRange.Type.Secondary("text", "html"), parameters = Parameters.Empty)),
//         NonEmptyList.of(
//           MediaRange(tpe = MediaRange.Type.Secondary("text", "plain"), parameters = Parameters.of(ci"foo" -> "bar")),
//           MediaRange(tpe = MediaRange.Type.Secondary("text", "plain"), parameters = Parameters.Empty),
//           MediaRange(tpe = MediaRange.Type.Primary("image"), parameters = Parameters.Empty),
//           MediaRange(tpe = MediaRange.Type.Primary("text"), parameters = Parameters.Empty),
//           MediaRange(tpe = MediaRange.Type.Secondary("application", "json"), parameters = Parameters.Empty),
//           MediaRange(tpe = MediaRange.Type.Any, parameters = Parameters.Empty)
//         )
//       )
//     )
