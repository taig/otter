package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.Reference

final class ReferenceDecoder[S[_], T](decoder: Decoder[S, T]):
  def apply[A](reference: Reference[S, A], value: T): Validated[Violations, A] =
    decoder.decode(schema = reference.value, value)
