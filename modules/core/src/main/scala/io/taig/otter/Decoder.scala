package io.taig.otter

import cats.data.Validated
import io.taig.otter.validation.Violations

trait Decoder[S[_], A, B]:
  def apply[C](schema: S[C], a: A): Validated[Violations[B, B], C]
