package io.taig.otter.codec

import cats.Functor
import cats.syntax.all.*
import io.taig.otter.Field
import io.taig.otter.Typescript

final class FieldTypescriptRenderer[S[_], T[_], U[_]: Functor](
    key: Encoder[S, String],
    value: Renderer[T, U[Typescript]]
) extends Renderer[Field[S, T, *], U[(String, Typescript)]]:
  override def render[A](schema: Field[S, T, A]): U[(String, Typescript)] = value
    .render(schema = schema.value.value.value)
    .tupleLeft(ReferenceConstantRenderer(encoder = key).render(schema.key))
