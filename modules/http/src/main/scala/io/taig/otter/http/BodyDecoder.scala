package io.taig.otter.http

import cats.data.Validated
import io.taig.otter.Violations

abstract class BodyDecoder[S[_]]:
  def apply[A](codec: S[A], bytes: Array[Byte]): Validated[Violations, A]
