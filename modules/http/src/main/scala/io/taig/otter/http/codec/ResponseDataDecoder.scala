package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.http.Headers.Data.contentType
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.Response

final class ResponseDataDecoder[-S[_]](decoder: PayloadDecoder[S]):
  val results = ResultsDataDecoder(decoder)

  def decode[A](
      schema: Response[S, A],
      value: Response.Data
  ): Either[ContentNegotiationFailed | Failure | MediaTypeUnsupported | ValidationViolations, A] =
    value.headers.contentType
      .leftMap("header" /: _)
      .leftMap(ValidationViolations.apply)
      .flatMap: contentType =>
        // TODO accumulate errors properly
        results.decode(schema = schema.results, contentType, value) match
          case Right(Some(a))        => a.asRight
          case Right(None) | Left(_) =>
            results.result.decode(schema = schema.validation, contentType, value) match
              case Right(Some(violations)) => Left(ValidationViolations(violations))
              case Right(None) | Left(_)   =>
                results.result.decode(schema = schema.failure, contentType, value) match
                  case Right(Some(failure)) => Failure(failure).asLeft
                  case Right(None)          => ContentNegotiationFailed.asLeft
                  case Left(error)          => error.asLeft
