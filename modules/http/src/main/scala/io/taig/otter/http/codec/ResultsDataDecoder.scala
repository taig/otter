package io.taig.otter.http.codec
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
  ): Either[ContentNegotiationFailed | MediaTypeUnsupported | ValidationViolations, A] =
    decode(schema = schema.value, contentType, data)

  def decode[A](
      schema: Results.Value[S, A],
      contentType: Option[MediaType],
      data: Response.Data
  ): Either[ContentNegotiationFailed | MediaTypeUnsupported | ValidationViolations, A] = schema match
    case Results.Value.Modify(self, f, _) => decode(schema = self, contentType, data).map(f)
    case Results.Value.OrElse(left, right) =>
      decode(schema = left, contentType, data)
        .map(Left(_))
        .orElse(decode(schema = right, contentType, data).map(Right(_)))
    case Results.Value.Root(result) => this.result.decode(result, contentType, data)
