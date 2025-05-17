// package io.taig.otter.http

// import cats.syntax.all.*
// import io.taig.otter.http.HttpError.*
// import io.taig.otter.http.header.MediaType

// final class ResultsDataDecoder[-S[_]](decoder: PayloadDecoder[S]):
//   val reader = ResultDataDecoder(decoder)

//   def apply[A](
//       results: Results[S, A],
//       contentType: Option[MediaType],
//       data: Response.Data
//   ): Either[ContentNegotiationFailed | MediaTypeUnsupported | ValidationViolations, A] = results match
//     case Results.Modify(self, f, _)  => apply(results = self, contentType, data).map(f)
//     case Results.OrElse(left, right) =>
//       // working around the compiler here
//       lazy val b = apply(results = right, contentType, data).map(_.asRight)
//       apply(results = left, contentType, data).map(_.asLeft).orElse(b)
//     case Results.Root(result) => reader(result, contentType, data)
