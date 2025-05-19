package io.taig.otter.codec

import io.taig.otter.ZodExpression
import io.taig.otter.ZodState
import io.taig.otter.Nullable
import cats.syntax.all.*
import cats.data.State

final class NullableZodRenderer[S[_]](renderer: Renderer[S, ZodState[ZodExpression]])
    extends Renderer[Nullable[S, *], ZodState[String]]:
  override def render[A](schema: Nullable[S, A]): ZodState[String] = schema match
    case Nullable.Modify(self, _, _) => render(schema = self)
    case Nullable.Default(reference, _, _) =>
      renderer.render(schema = reference.value).map(expression => show"z.nullable($expression)")
    case Nullable.Root(reference, _) =>
      renderer.render(schema = reference.value).map(expression => show"z.nullable($expression)")
    case Nullable.Void(metadata) => State.pure("z.void()")
