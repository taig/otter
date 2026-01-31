// package io.taig.otter.http.codec

// import io.taig.otter.http.Http
// import cats.data.Chain
// import io.taig.otter.codec.Decoder
// import io.taig.otter.Violations
// import io.taig.validation.Violation
// import io.taig.otter.Constraint
// import cats.syntax.all.*

// val HttpPathDecoder: Decoder[Http.Path.Read, Chain[String]] = PathDecoder(parser = HttpSegmentParser)
//   .verify: remainders =>
//     Option.when(remainders.nonEmpty):
//       Violations(
//         Violation(
//           constraint = Constraint.Generic.Equals(reference = "/"),
//           actual = remainders.mkString_("/"),
//           hint = none
//         )
//       )
//   .contramapK([_] => _.self.self)
