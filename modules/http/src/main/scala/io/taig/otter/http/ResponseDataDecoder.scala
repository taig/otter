package io.taig.otter.http

import io.taig.otter.+
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.http.Response.Error.ContentNegotiationFailed

final class ResponseDataDecoder[-S[_], -T[_]](decoder: PayloadDecoder[S + T]):
  def apply[A](response: Response[S, T, A], data: Response.Data): Either[ContentNegotiationFailed, A] =
    response.result
    ???
