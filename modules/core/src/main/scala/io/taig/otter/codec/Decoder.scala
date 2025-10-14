package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Violation
import io.taig.otter.Violations

trait Decoder[-S[_], T]:
  def decode[A](schema: S[A], value: T): Validated[Violations, A]
