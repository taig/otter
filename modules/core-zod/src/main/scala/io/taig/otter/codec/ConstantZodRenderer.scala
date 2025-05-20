package io.taig.otter.codec

import io.taig.otter.Constant
import cats.syntax.all.*
import cats.Functor

final class ConstantZodRenderer[S[_], T[_]: Functor](printer: Encoder[S, T[String]])
    extends Renderer[Constant[S, *], T[String]]:
  override def render[A](schema: Constant[S, A]): T[String] =
    ReferenceConstantRenderer(encoder = printer)
      .render(reference = schema.schema)
      .map(expression => s"z.literal($expression)")
