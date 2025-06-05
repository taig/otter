package io.taig.otter.codec

import cats.Functor
import cats.syntax.all.*
import io.taig.otter.Constant
import io.taig.otter.Typescript

final class ConstantTypescriptRenderer[S[_], T[_]: Functor](printer: Encoder[S, T[String]])
    extends Renderer[Constant[S, *], T[Typescript[Nothing]]]:
  override def render[A](schema: Constant[S, A]): T[Typescript[Nothing]] =
    ReferenceConstantRenderer(encoder = printer)
      .render(reference = schema.value.schema)
      .map(Typescript.Literal.apply)
