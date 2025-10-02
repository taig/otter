package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Violation

trait Decoder[-S[_], A]:
  def decode[A](schema: S[A], value: String): Validated[Violation, A]
