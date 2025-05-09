package io.taig.otter.http

import cats.data.Validated
import io.taig.otter.Violations

final class HttpResponseDecoder[S[_], T[_]]:
  def apply[A](response: Response[S, T, A], bytes: Array[Byte]): Validated[Violations, A] = ???
