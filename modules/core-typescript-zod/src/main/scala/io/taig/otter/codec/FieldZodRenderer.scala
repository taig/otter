package io.taig.otter.codec

import cats.syntax.all.*
import io.taig.otter.Field
import io.taig.otter.ZodExpression
import io.taig.otter.ZodState

final class FieldZodRenderer[S[_], T[_]](key: Encoder[S, String], value: Renderer[T, ZodState[ZodExpression]])
    extends Renderer[Field[S, T, *], ZodState[(String, ZodExpression)]]:
  override def render[A](schema: Field[S, T, A]): ZodState[(String, ZodExpression)] = value
    .render(schema = schema.value.value)
    .map:
      case expression if schema.isOptional => ZodExpression.Inline(show"z.optional($expression)")
      case expression                      => expression
    .tupleLeft(ReferenceConstantRenderer(encoder = key).render(reference = schema.key))
