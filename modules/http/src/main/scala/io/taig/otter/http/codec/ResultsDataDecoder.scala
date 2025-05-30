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
  ): Either[MediaTypeUnsupported | ValidationViolations, Option[A]] =
    decode(schema = schema.value, contentType, data)

  private def decode[A](
      schema: Results.Value[S, A],
      contentType: Option[MediaType],
      data: Response.Data
  ): Either[MediaTypeUnsupported | ValidationViolations, Option[A]] = schema match
    case Results.Value.Modify(self, f, _) => decode(schema = self, contentType, data).map(_.map(f))
    case Results.Value.OrElse(left, right) =>
      decode(schema = left, contentType, data) match
        case Right(Some(a)) => a.asLeft.some.asRight
        case Right(None) =>
          decode(schema = right, contentType, data) match
            case Right(Some(b)) => b.asRight.some.asRight
            case Right(None)    => none.asRight
            case Left(error)    => error.asLeft
        case Left(error) => error.asLeft
    case Results.Value.Root(result) => this.result.decode(result, contentType, data)
