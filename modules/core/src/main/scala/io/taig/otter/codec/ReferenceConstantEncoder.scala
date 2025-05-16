package io.taig.otter.codec

import io.taig.otter.Reference

final class ReferenceConstantEncoder[S[_], T](encoder: Encoder[S, T]):
  def apply[A](reference: Reference.Constant[S, A]): T = encoder.encode(schema = reference.self.value, reference.value)
