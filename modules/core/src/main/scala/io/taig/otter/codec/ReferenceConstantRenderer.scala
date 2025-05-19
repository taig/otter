package io.taig.otter.codec

import io.taig.otter.Reference

final class ReferenceConstantRenderer[S[_], T](encoder: Encoder[S, T]) extends Renderer[Reference.Constant[S, *], T]:
  override def render[A](reference: Reference.Constant[S, A]): T =
    encoder.encode(schema = reference.self.value, reference.value)
