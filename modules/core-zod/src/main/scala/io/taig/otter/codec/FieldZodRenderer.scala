package io.taig.otter.codec

import io.taig.otter.ZodState
import io.taig.otter.ZodExpression
import io.taig.otter.Field
import cats.syntax.all.*

final class FieldZodRenderer[S[_], T[_]](key: Encoder[S, String], value: Renderer[T, ZodState[ZodExpression]])
    extends Renderer[Field[S, T, *], ZodState[(String, ZodExpression)]]:
  override def render[A](schema: Field[S, T, A]): ZodState[(String, ZodExpression)] = value
    .render(schema = schema.value.value)
    .tupleLeft(ReferenceConstantRenderer(encoder = key).render(reference = schema.key))
