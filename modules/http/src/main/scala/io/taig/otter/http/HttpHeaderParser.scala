package io.taig.otter.http

import io.taig.otter.Violations
import cats.data.Validated

object HttpHeaderParser:
  def apply[A](header: Http.Header[A], value: String): Validated[Violations, A] = header match
    case codec: Http.Header.Value[A] => ???
