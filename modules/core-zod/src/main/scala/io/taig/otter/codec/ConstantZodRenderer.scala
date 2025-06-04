package io.taig.otter.codec

import cats.Functor
import cats.syntax.all.*
import io.taig.otter.Constant
import io.taig.otter.Zod

final class ConstantZodRenderer[S[_], T[_]: Functor](printer: Encoder[S, T[String]])
    extends Renderer[Constant[S, *], T[Zod]]:
  override def render[A](schema: Constant[S, A]): T[Zod] =
    ReferenceConstantRenderer(encoder = printer).render(reference = schema.value.schema).map(Zod.Literal.apply)
