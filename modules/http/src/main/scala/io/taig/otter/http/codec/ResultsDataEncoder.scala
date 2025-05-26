// package io.taig.otter.http.codec

// import io.taig.otter.http.HttpError.ContentNegotiationFailed
// import io.taig.otter.http.Response
// import io.taig.otter.http.Results
// import io.taig.otter.http.header.Accept

// final class ResultsDataEncoder[-S[_]](encoder: PayloadEncoder[S]):
//   val result = ResultDataEncoder(encoder)

//   def encode[A](
//       schema: Results[S, A],
//       accept: Option[Accept],
//       a: A
//   ): Either[ContentNegotiationFailed, Response.Data] = schema match
//     case Results.Modify(self, _, g)  => encode(schema = self, accept, g(a))
//     case Results.OrElse(left, right) => a.fold(encode(schema = left, accept, _), encode(schema = right, accept, _))
//     case Results.Root(result)        => this.result.encode(result, accept, a)
