package io.taig.otter.codec

import io.taig.otter.Reference

final class ReferenceEncoder[S[_], T](encoder: Encoder[S, T]):
  def apply[A](reference: Reference[S, A], a: A): T = encoder(schema = reference.value, a)
