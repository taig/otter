package io.taig.otter.http

import cats.data.Validated
import io.taig.otter.Violations

final class HttpRequestDecoder[S[_]]:
  def apply[A](request: Request[S, A], bytes: Array[Byte]): Validated[Violations, A] = ???
