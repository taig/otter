package io.taig.otter.codec

import cats.Functor
import cats.syntax.all.*
import io.taig.otter.Constant
import io.taig.otter.Typescript

final class ConstantTypescriptRenderer[S[_]](printer: Encoder[S, String])
    extends Renderer[Constant[S, *], Typescript[Nothing]]:
  override def render[A](schema: Constant[S, A]): Typescript[Nothing] =
    val value = ReferenceConstantRenderer(encoder = printer).render(reference = schema.value.schema)
    Typescript.Literal(value)
