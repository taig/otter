package io.taig.otter.http

import io.taig.otter.+
import cats.data.Validated
import io.taig.otter.Violations

final class ResponseDataDecoder[S[_], T[_]](decoder: PayloadDecoder[S + T]):
  def apply[A](response: Response[S, T, A], data: Response.Data): Either[Throwable, Validated[Request.Error, A]] = ???
