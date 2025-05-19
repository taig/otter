package io.taig.otter.codec

import io.taig.otter.Constant

final class ConstantZodRenderer[S[_]](printer: Encoder[S, String]) extends Renderer[Constant[S, *], String]:
  override def render[A](schema: Constant[S, A]): String =
    s"z.literal(${ReferenceConstantRenderer(encoder = printer).render(schema.schema)})"
