// package io.taig.otter.http.codec

// import cats.data.Chain
// import cats.syntax.all.*
// import io.taig.otter.+
// import io.taig.otter.StacktracePrinter
// import io.taig.otter.Violations
// import io.taig.otter.http.Headers
// import io.taig.otter.http.Headers.Data.accept
// import io.taig.otter.http.HttpError.*
// import io.taig.otter.http.Response
// import io.taig.otter.http.header.Accept
// import io.taig.otter.http.syntax.CodeSyntax.*

// final class ResponseDataEncoder[S[_]](encoder: PayloadEncoder[S], debug: Boolean):
//   val results = ResultsDataEncoder(encoder)

//   def encode[A](
//       schema: Response[S, A],
//       headers: Headers.Data,
//       result: Either[Failure | MediaTypeUnsupported | ValidationViolations, A]
//   ): Response.Data = headers.accept
//     .leftMap("header" /: _)
//     .leftMap(ValidationViolations.apply)
//     .fold(
//       error => encode(schema, accept = none, result = error.asLeft),
//       encode(schema, _, result)
//     )

//   def encode[A](
//       schema: Response[S, A],
//       accept: Option[Accept],
//       result: Either[Failure | MediaTypeUnsupported | ValidationViolations, A]
//   ): Response.Data = ???
//   // result
//   //   .match
//   //     case Right(a) => results.encode(schema = schema.results, accept, a)
//   //     case Left(Failure(throwable)) =>
//   //       results.result.encode(schema = schema.failure, accept, Option.when(debug)(StacktracePrinter(throwable)))
//   //     case Left(MediaTypeUnsupported) =>
//   //       Response.Data(code = code.unsupportedMediaTypes, headers = Chain.empty, body = Array.emptyByteArray).asRight
//   //     case Left(ValidationViolations(violations)) =>
//   //       results.result.encode(schema = schema.validation, accept, violations)
//   //   .getOrElse(Response.Data(code = code.notAcceptable, headers = Chain.empty, body = Array.emptyByteArray))
