package io.taig.otter.codec

import io.taig.otter.Constant
import io.taig.otter.Effect

final class ConstantEffectRenderer[S[_]](printer: Encoder[S, String]) extends Renderer[Constant[S, *], Effect.Literal]:
  override def render[A](schema: Constant[S, A]): Effect.Literal =
    val value = ReferenceConstantRenderer(encoder = printer).render(reference = schema.value.schema)
    Effect.Literal(value)
