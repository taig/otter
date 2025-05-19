package io.taig.otter.codec

import cats.data.State
import io.taig.otter.Constant
import io.taig.otter.ZodState

final class ConstantZodRenderer[S[_]](
    printer: Encoder[S, Option[String]],
    renderer: Renderer[S, ZodState[String]]
) extends Renderer[Constant[S, *], ZodState[String]]:
  override def render[A](schema: Constant[S, A]): ZodState[String] =
    ReferenceConstantRenderer(encoder = printer).render(schema.schema) match
      case Some(value) => State.pure(s"z.literal($value)")
      case None        => renderer.render(schema = schema.schema.self.value)
