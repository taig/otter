package io.taig.otter.codec

import cats.Functor
import cats.syntax.all.*
import io.taig.otter.Field
import io.taig.otter.Zod

final class FieldZodRenderer[S[_], T[_], U[_]: Functor](
    key: Encoder[S, String],
    value: Renderer[T, U[Zod]]
) extends Renderer[Field[S, T, *], U[(String, Zod)]]:
  override def render[A](schema: Field[S, T, A]): U[(String, Zod)] = value
    .render(schema = schema.value.value.value)
    .tupleLeft(ReferenceConstantRenderer(encoder = key).render(schema.key))
