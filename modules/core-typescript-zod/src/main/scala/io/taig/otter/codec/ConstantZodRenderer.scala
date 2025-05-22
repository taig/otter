package io.taig.otter.codec

import cats.Functor
import cats.syntax.all.*
import io.taig.otter.Constant

final class ConstantZodRenderer[S[_], T[_]: Functor](printer: Encoder[S, T[String]])
    extends Renderer[Constant[S, *], T[String]]:
  override def render[A](schema: Constant[S, A]): T[String] =
    ReferenceConstantRenderer(encoder = printer)
      .render(reference = schema.schema)
      .map(expression => s"z.literal($expression)")
