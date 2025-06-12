package io.taig.otter.codec

import io.taig.otter.Constant
import io.taig.otter.Typescript

final class ConstantTypescriptRenderer[S[_]](printer: Encoder[S, String])
    extends Renderer[Constant[S, *], Typescript.Literal]:
  override def render[A](schema: Constant[S, A]): Typescript.Literal =
    val value = ReferenceConstantRenderer(encoder = printer).render(reference = schema.value.schema)
    Typescript.Literal(value)
