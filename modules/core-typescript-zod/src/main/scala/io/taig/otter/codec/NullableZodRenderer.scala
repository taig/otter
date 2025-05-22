package io.taig.otter.codec

import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Nullable
import io.taig.otter.ZodExpression
import io.taig.otter.ZodState

final class NullableZodRenderer[S[_]](renderer: Renderer[S, ZodState[ZodExpression]])
    extends Renderer[Nullable[S, *], ZodState[String]]:
  override def render[A](schema: Nullable[S, A]): ZodState[String] =
    schema.schema.fold(State.pure("z.void()")): schema =>
      renderer.render(schema = schema.value).map(expression => show"z.nullable($expression)")
