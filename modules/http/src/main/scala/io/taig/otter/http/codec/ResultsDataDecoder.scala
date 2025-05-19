package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.Response
import io.taig.otter.http.Results
import io.taig.otter.http.header.MediaType

final class ResultsDataDecoder[-S[_]](decoder: PayloadDecoder[S]):
  val result = ResultDataDecoder(decoder)

  def decode[A](
      schema: Results[S, A],
      contentType: Option[MediaType],
      data: Response.Data
  ): Either[ContentNegotiationFailed | MediaTypeUnsupported | ValidationViolations, A] = schema match
    case Results.Modify(self, f, _)  => decode(schema = self, contentType, data).map(f)
    case Results.OrElse(left, right) =>
      // working around the compiler here
      lazy val b = decode(schema = right, contentType, data).map(_.asRight)
      decode(schema = left, contentType, data).map(_.asLeft).orElse(b)
    case Results.Root(result) => this.result.decode(result, contentType, data)
