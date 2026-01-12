package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Violations

trait Decoder[F[_], A]:
  def decode[B](fb: F[B], a: A): Validated[Violations, B]
